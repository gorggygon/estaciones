
# -*- coding: utf-8 -*-
"""
Script de verificación de consistencia entre Excel y PDFs
"""

from pathlib import Path
import pandas as pd

# Rutas relativas del proyecto
BASE_DIR = Path("f:/proyectos/contraloria_2026_419est/")
EXCEL_PATH = BASE_DIR / "ESTADO_RED_CTR.xlsx"
PDF_DIR = BASE_DIR / "contr_imagenes2"

def verificar_archivos():
    print("=" * 60)
    print("VERIFICADOR DE ESTACIONES Y REPORTES PDF")
    print("=" * 60)

    # 1. Verificar si existen los archivos principales
    if not EXCEL_PATH.exists():
        print(f"[ERROR] No se encuentra el archivo Excel en: {EXCEL_PATH}")
        return
    
    if not PDF_DIR.exists():
        print(f"[ERROR] No se encuentra la carpeta de PDFs en: {PDF_DIR}")
        return

    print(f"-> Excel encontrado: {EXCEL_PATH.name}")
    print(f"-> Carpeta de PDFs encontrada: {PDF_DIR.name}\n")

    # 2. Cargar la hoja de verificación del Excel
    try:
        df = pd.read_excel(EXCEL_PATH, sheet_name="verificacion", engine="openpyxl")
        df.columns = df.columns.str.strip()
    except Exception as e:
        print(f"[ERROR] No se pudo leer la hoja 'verificacion' del Excel: {e}")
        return

    # 3. Validar columnas necesarias
    if "Nombre" not in df.columns:
        print("[ERROR] La columna 'Nombre' no existe en la hoja 'verificacion'.")
        return

    columna_estado = [c for c in df.columns if "estado" in c.lower()]
    if not columna_estado:
        print("[ERROR] No se encontró una columna de estado en el Excel.")
        return
    col_estado = columna_estado[0]

    # Filtrar estaciones no suspendidas (igual que en tu app)
    df_activas = df[df[col_estado].str.upper() != "SUSPENDIDO"].copy()
    
    total_estaciones = len(df_activas)
    encontrados = 0
    faltantes = 0
    lista_faltantes = []

    print(f"Analizando {total_estaciones} estaciones activas...\n")

    # 4. Comprobar existencia de cada PDF
    for _, row in df_activas.iterrows():
        nombre_base = str(row["Nombre"]).strip()
        nombre_pdf = f"{nombre_base}_.pdf"
        ruta_pdf = PDF_DIR / nombre_pdf

        if ruta_pdf.exists():
            encontrados += 1
        else:
            faltantes += 1
            lista_faltantes.append(nombre_pdf)

    # 5. Mostrar resultados
    print("-" * 60)
    print(f"📊 RESULTADOS DE LA VERIFICACIÓN:")
    print(f"   • Total de estaciones analizadas: {total_estaciones}")
    print(f"   • PDF's existentes y enlazados correctamente: {encontrados} ✅")
    print(f"   • PDF's faltantes en la carpeta: {faltantes} ❌")
    print("-" * 60)

    if faltantes > 0:
        print("\n⚠️ Los siguientes archivos PDF no se encontraron en la carpeta:")
        for f in lista_faltantes[:]:  # Muestra los primeros 15 para no saturar la pantalla
            print(f"   - {f}")
        if len(lista_faltantes) > 15:
            print(f"   ... y {len(lista_faltantes) - 15} archivos más.")
    else:
        print("\n🎉 ¡Excelente! Todas las estaciones tienen su respectivo PDF en la carpeta.")

if __name__ == "__main__":
    verificar_archivos()