# -*- coding: utf-8 -*-
"""
Created on Sat Oct 10 09:36:43 2026

@author: jagonzalez
"""

import math
from pathlib import Path

import fitz  # PyMuPDF para convertir PDF a imagen en memoria
import folium
import pandas as pd
import streamlit as st
from streamlit_folium import st_folium

# ----------------------------------------------------------------------------
# 1. CONFIGURACIÓN DE PÁGINA (¡DEBE SER LO PRIMERO DE STREAMLIT!)
# ----------------------------------------------------------------------------
st.set_page_config(
    page_title="Verificación información estaciones_IDEAM",  
    layout="wide"
)

# ----------------------------------------------------------------------------
# Rutas y Constantes (Consistentes para Local y Cloud)
# ----------------------------------------------------------------------------
BASE_DIR = Path(__file__).parent
EXCEL_PATH = BASE_DIR / "ESTADO_RED_CTR.xlsx"
PDF_DIR = BASE_DIR / "contr_imagenes2"
CENTRO_COLOMBIA = [4.5709, -74.2973]


@st.cache_data(show_spinner=False)
def cargar_puntos() -> pd.DataFrame:
    if EXCEL_PATH.exists():
        df = pd.read_excel(EXCEL_PATH, sheet_name="verificacion", engine="openpyxl")
        df.columns = df.columns.str.strip()

        # Asegurar que exista NOMBRE_CNE
        if "NOMBRE_CNE" not in df.columns:
            posibles = [c for c in df.columns if "cne" in c.lower()]
            if posibles:
                df["NOMBRE_CNE"] = df[posibles[0]]

        df = df.rename(columns={
            "Codigo": "id", 
            "Municipio": "municipio",
            "Departamento": "departamento",
            "Categoría": "categoria",
            "LATITUD": "lat",
            "LONGITUD": "lon",
            "FAMILIA": "FAMILIA",
            "TX actual 2026":"Transmisión",
            "TECN": "TECNOLOGIA"
        })

        df["lat"] = pd.to_numeric(df["lat"], errors="coerce")
        df["lon"] = pd.to_numeric(df["lon"], errors="coerce")
        df = df.dropna(subset=["lat", "lon"])

        columna_estado = [c for c in df.columns if "estado" in c.lower()][0]

        df["descripcion"] = (
            df["municipio"].fillna("") + " (" + df["departamento"].fillna("") + ") · "+df["FAMILIA"]+ " · "
            + df["categoria"].fillna("") + " · " + df[columna_estado].fillna("")+ " · " +df["Transmisión"]
        )

        df["id"] = df["id"].astype(str).str.split('.').str[0]
        df["pdf"] = df["Nombre"].astype(str) + "_.pdf"
        df = df[df[columna_estado].str.upper() != "SUSPENDIDO"]

        return df
    return pd.DataFrame()


@st.cache_data(show_spinner=False)
def cargar_datos_variables() -> pd.DataFrame:
    """Carga la hoja 'datos' del Excel para extraer conteos por variable."""
    if EXCEL_PATH.exists():
        try:
            df_datos = pd.read_excel(EXCEL_PATH, sheet_name="datos", engine="openpyxl")
            df_datos.columns = df_datos.columns.str.strip()
            return df_datos
        except Exception:
            return pd.DataFrame()
    return pd.DataFrame()


# ----------------------------------------------------------------------------
# Utilidades
# ----------------------------------------------------------------------------
def punto_mas_cercano(df: pd.DataFrame, lat: float, lon: float, tol_km: float = 5.0):
    def haversine(la1, lo1, la2, lo2):
        r = 6371.0
        p1, p2 = math.radians(la1), math.radians(la2)
        dp, dl = math.radians(la2 - la1), math.radians(lo2 - lo1)
        a = math.sin(dp / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dl / 2) ** 2
        return 2 * r * math.asin(math.sqrt(a))

    dist = df.apply(lambda f: haversine(lat, lon, f["lat"], f["lon"]), axis=1)
    idx = dist.idxmin()
    return df.loc[idx] if dist[idx] <= tol_km else None


def mostrar_pdf_como_imagen(ruta: Path) -> None:
    try:
        doc = fitz.open(str(ruta))
        page = doc[0]
        pix = page.get_pixmap(dpi=150)
        img_bytes = pix.tobytes("png")
        
        st.image(img_bytes, caption="Reporte de Estación", use_container_width=True)
        
        datos_pdf = ruta.read_bytes()
        st.download_button(
            "⬇️ Descargar PDF Original", 
            data=datos_pdf, 
            file_name=ruta.name,
            mime="application/pdf", 
            use_container_width=True
        )
    except Exception as e:
        st.error(f"No se pudo procesar el reporte: {e}")


def construir_mapa(df: pd.DataFrame, seleccionado_id=None, color_por="FAMILIA") -> folium.Map:
    m = folium.Map(location=CENTRO_COLOMBIA, zoom_start=5, tiles=None)
    folium.TileLayer("OpenStreetMap", name="OpenStreetMap").add_to(m)
    folium.TileLayer(
        tiles="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        attr="Tiles &copy; Esri", name="Satélite (Esri)",
    ).add_to(m)
    folium.TileLayer(
        tiles="https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
        attr="&copy; OpenTopoMap (CC-BY-SA)", name="Topográfico",
    ).add_to(m)
    
    COLORES = {
        "PLUVIOMETRICAS": "#1f77b4", "HIDROLOGICAS": "#2ca02c",
        "CLIMATOLOGICAS": "#ff7f0e", "RADIOSONDAS": "#9467bd",
        "CONVENCIONAL": "#1f77b4", "AUTOMATICA": "#d62728",
        "HIDROLOGICA": "#2ca02c", "CLIMATOLOGICA": "#ff7f0e",
    }
    grupos = {}
    for _, f in df.iterrows():
        cat = str(f[color_por]) if pd.notna(f[color_por]) else "SIN DATO"
        col = COLORES.get(cat.upper(), "gray")
        if cat not in grupos:
            grupos[cat] = folium.FeatureGroup(
                name=f'<span style="color:{col};font-size:16px">●</span> {cat.capitalize()}'
            ).add_to(m)
        activo = f["id"] == seleccionado_id
        folium.CircleMarker(
            location=[f["lat"], f["lon"]],
            radius=8 if activo else 4,
            color="black" if activo else col,
            weight=3 if activo else 1,
            fill=True, fill_color=col, fill_opacity=0.8,
            tooltip=f"{f.get('NOMBRE_CNE', '')} · {f.get('FAMILIA', '')} · {f.get('TECNOLOGIA', '')}",
        ).add_to(grupos[cat])

    folium.LayerControl(position="topright", collapsed=False).add_to(m)
    return m


# ----------------------------------------------------------------------------
# Interfaz de Usuario
# ----------------------------------------------------------------------------
st.title("🗺️ Mapa de puntos en Colombia")
st.caption("Haz clic en un punto en el mapa o selecciónalo en la lista desplegable.")

df = cargar_puntos()
df_datos = cargar_datos_variables()

if "seleccion" not in st.session_state:
    st.session_state.seleccion = None

col_mapa, col_info = st.columns([1, 1], gap="medium")

with col_mapa:
    opcion = st.radio("Diferenciar por", ["Familia", "Tecnología"], horizontal=True)
    color_por = "FAMILIA" if opcion == "Familia" else "TECNOLOGIA"
    mapa = construir_mapa(df, st.session_state.seleccion, color_por)
    
    salida = st_folium(mapa, use_container_width=True, height=750, key="mapa")

    clic = (salida or {}).get("last_object_clicked")
    if clic:
        fila = punto_mas_cercano(df, clic["lat"], clic["lng"])
        if fila is not None and fila["id"] != st.session_state.seleccion:
            st.session_state.seleccion = fila["id"]
            st.rerun()

with col_info:
    # --- LISTA DESPLEGABLE PARA BUSCAR ESTACIÓN ---
    df_sorted = df.sort_values("NOMBRE_CNE")
    nombres_estaciones = ["-- Selecciona una estación --"] + list(df_sorted["NOMBRE_CNE"].unique())
    
    # Encontrar el nombre actual si hay una selección activa
    nombre_actual = "-- Selecciona una estación --"
    if st.session_state.seleccion is not None:
        match_actual = df[df["id"] == st.session_state.seleccion]["NOMBRE_CNE"]
        if not match_actual.empty:
            nombre_actual = match_actual.values[0]

    estacion_elegida = st.selectbox(
        "🔍 Buscar estación por nombre",
        nombres_estaciones,
        index=nombres_estaciones.index(nombre_actual) if nombre_actual in nombres_estaciones else 0
    )

    # Si el usuario selecciona una estación distinta en el menú
    if estacion_elegida != "-- Selecciona una estación --":
        fila_seleccionada_menu = df_sorted[df_sorted["NOMBRE_CNE"] == estacion_elegida].iloc[0]
        nuevo_id = fila_seleccionada_menu["id"]
        if nuevo_id != st.session_state.seleccion:
            st.session_state.seleccion = nuevo_id
            st.rerun()

    # --- MOSTRAR INFORMACIÓN DE LA ESTACIÓN SELECCIONADA ---
    sel = st.session_state.seleccion
    if sel is None:
        st.info("👈 Selecciona un punto en el mapa o usa el buscador de arriba.")
        st.dataframe(df[["NOMBRE_CNE", "lat", "lon"]].rename(columns={"NOMBRE_CNE": "Estación"}), hide_index=True, use_container_width=True)
    else:
        fila = df[df["id"] == sel].iloc[0]
        st.subheader(fila["NOMBRE_CNE"])
        st.write(fila["descripcion"])
        
        c1, c2 = st.columns(2)
        c1.metric("Latitud", f"{fila['lat']:.4f}")
        c2.metric("Longitud", f"{fila['lon']:.4f}")

        if not df_datos.empty:
            cols_match = [c for c in df_datos.columns if "nom_auto" in c.lower() or "cne" in c.lower() or "estacion" in c.lower()]
            
            if cols_match:
                col_estacion = cols_match[0]
                id_estacion = str(fila["id"]).strip()
                match_datos = df_datos[df_datos[col_estacion].astype(str).str.contains(id_estacion, case=False, na=False)]
                
                if match_datos.empty and "NOM_AUTO" in df.columns:
                    nom_auto_val = str(fila["NOM_AUTO"]).strip()
                    match_datos = df_datos[df_datos[col_estacion].astype(str).str.contains(nom_auto_val, case=False, na=False)]
                
                if not match_datos.empty:
                    reg = match_datos.iloc[0]
                    detalles_variables = []
                    total_datos = 0
                    
                    col_total_candidatas = [c for c in df_datos.columns if "total" in c.lower()]
                    col_total_real = col_total_candidatas[0] if col_total_candidatas else None
                    
                    for col in df_datos.columns:
                        if col == col_total_real:
                            try:
                                val_tot = reg[col]
                                if pd.notna(val_tot):
                                    total_datos = int(float(str(val_tot).replace(",", "").strip()))
                            except Exception:
                                pass
                            continue
                            
                        if col != col_estacion:
                            val = reg[col]
                            if pd.notna(val):
                                try:
                                    num_val = float(str(val).replace(",", "").strip())
                                    if num_val > 0:
                                        detalles_variables.append(f"**{col.replace('_', ' ').title()}** ({int(num_val):,} registros)")
                                except Exception:
                                    pass

                    titulo_seccion = f"Total datos analizados: {total_datos:,}" if total_datos > 0 else "📊 Resumen de Datos Analizados"
                    st.markdown(f"### {titulo_seccion}")
                    
                    if detalles_variables:
                        texto_vars = ", ".join(detalles_variables[:-1]) + (" y " if len(detalles_variables) > 1 else "") + detalles_variables[-1] if len(detalles_variables) > 1 else detalles_variables[0]
                        st.write(f"Durante el periodo de análisis del año 2023 a la fecha, la estación registró información para las siguientes variables: {texto_vars}.")
                else:
                    st.info("ℹ️ No se encontraron registros de conteo para esta estación en la hoja 'datos'.")
        st.markdown("---")
        ruta_pdf = PDF_DIR / str(fila["pdf"])
        if ruta_pdf.exists():
            mostrar_pdf_como_imagen(ruta_pdf)
        else:
            st.warning(f"No se encontró el reporte en PDF: {ruta_pdf.name}")

        if st.button("Limpiar selección"):
            st.session_state.seleccion = None
            st.rerun()