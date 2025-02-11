import tkinter as tk
from tkinter import ttk
import os
import clipboard
import subprocess

KEYS_DIR = "keys"
OUTPUT_DIR = "output"
CURRENT_PAGE = 0
ITEMS_PER_PAGE = 50

def list_keys():
    return [f[:-2] for f in os.listdir(KEYS_DIR) if f.endswith('.R')]

def list_glaciers_from_dem():
    dem_dir = "data/DigitalElevationModels"
    glacier_files = [f for f in os.listdir(dem_dir) if f.endswith('_NASADEM.tif')]
    glacier_names = [f.split('_NASADEM.tif')[0] for f in glacier_files]
    return glacier_names

def run_script_for_selected_glaciers(step):
    step_dir = os.path.join(OUTPUT_DIR, selected_key.get(), step)
    glacier_list_path = os.path.join(step_dir, "glacier_list.R")
    glacier_list = selected_glaciers.get(step, [])

    with open(glacier_list_path, 'a') as f:
        f.write("glacier_list <- c({})\n".format(','.join(f"'{g}'" for g in glacier_list)))

    command = f"Rscript main.R"
    subprocess.run(f"cmd.exe /c start cmd.exe /k {command}", cwd=step_dir, shell=True)

def list_glaciers(key):
    key_dir = os.path.join(OUTPUT_DIR, key)
    steps = [d for d in os.listdir(key_dir) if os.path.isdir(os.path.join(key_dir, d))]
    glacier_set = set()

    valid_suffixes = {
        '_dem.tif', '_coord_parallel.rds', '_al.rds', '_ts_intensity.rds', '_dates_cut.rds',
        '_outlier.rds', '_indices_to_remove.rds', '_candidate_paths.rds', '_path_costs.rds',
        '_sSmooth.rds', '_min_cost_indices.rds', '_smoothed_paths.rds', '_fda_results.rds'
    }

    for step in steps:
        step_dir = os.path.join(key_dir, step, "output")
        if os.path.exists(step_dir):
            glaciers = {
                file.split('_')[0]
                for file in os.listdir(step_dir)
                if any(file.endswith(suffix) for suffix in valid_suffixes)
            }
            glacier_set.update(glaciers)
    return list(glacier_set), steps

required_files = {
    '01_prepare_dem': lambda glacier: [f"{glacier}_dem.tif"],
    '02_GD_flowline': lambda glacier: [f"{glacier}_coord_parallel.rds",f"{glacier}_initial_coord.rds"],
    '03_extract_IP': lambda glacier: [
        f"{glacier}_al.rds", f"{glacier}_ts_intensity.rds", f"{glacier}_dates_cut.rds",
        f"{glacier}_outlier.rds", f"{glacier}_indices_to_remove.rds"
    ],
    '04_candidate_paths': lambda glacier: [
        f"{glacier}_candidate_paths.rds", f"{glacier}_path_costs.rds", f"{glacier}_sSmooth.rds"
    ],
    '05_clustering': lambda glacier: [f"{glacier}_min_cost_indices.rds"],
    '06_smoothing': lambda glacier: [f"{glacier}_smoothened_paths.rds"],
    '07_fda': lambda glacier: [f"{glacier}_fda_results.rds"],
    '08_make_gifs': lambda glacier: [f"{glacier}_fda_results.rds"]
}


def check_files_present(glacier, step, key):
    step_dir = os.path.join(OUTPUT_DIR, key, step, "output")
    if not os.path.exists(step_dir):
        return False
    
    existing_files = set()
    for root, dirs, files in os.walk(step_dir):
        for file in files:
            existing_files.add(file)
    
    return all(rf in existing_files for rf in required_files[step](glacier))


selected_glaciers = {}

def on_key_selected(event):
    selected_key.set(key_dropdown.get())
    search_var.set("")
    initialize_table(selected_key.get())

def initialize_table(key):
    glaciers = list_glaciers_from_dem()
    _, steps = list_glaciers(key)

    update_table.glaciers = glaciers
    update_table.steps = steps

    if not hasattr(update_table, "buttons"):
        update_table.buttons = {}
    if not hasattr(update_table, "labels"):
        update_table.labels = {}

    for widget in table_frame.winfo_children():
        widget.destroy()

    tk.Label(table_frame, text="Glacier Names", font="Helvetica 10 bold").grid(row=0, column=0)
    for idx, step in enumerate(steps, start=1):
        tk.Label(table_frame, text=step, font="Helvetica 10 bold").grid(row=0, column=idx)
        btn = tk.Button(table_frame, text="Run Script for Selected Glaciers",
                        command=lambda step=step: run_script_for_selected_glaciers(step))
        btn.grid(row=1, column=idx)
        update_table.buttons[step] = btn

    sorted_glaciers = sorted(glaciers, key=lambda g: [check_files_present(g, step, key) for step in steps], reverse=True)

    update_table.sorted_glaciers = sorted_glaciers

    for glacier in sorted_glaciers:
        update_table.labels[glacier] = {"name": tk.Label(table_frame, text=glacier, font="Helvetica 10")}
        for step in steps:
            present = check_files_present(glacier, step, key)
            color = 'green' if present else 'red'
            lbl = tk.Label(table_frame, text=glacier, bg=color, relief='raised')
            lbl.bind("<1>", lambda e, glacier=glacier, step=step: toggle_glacier_selection(glacier, step))
            update_table.labels[glacier][step] = lbl

    update_table_page(0)

def update_table_page(page):
    global CURRENT_PAGE
    CURRENT_PAGE = page

    glaciers = update_table.sorted_glaciers
    steps = update_table.steps

    start_idx = page * ITEMS_PER_PAGE
    end_idx = start_idx + ITEMS_PER_PAGE
    paginated_glaciers = glaciers[start_idx:end_idx]

    for widget in table_frame.winfo_children():
        if isinstance(widget, tk.Label) and widget.cget("text") not in ["Glacier Names"] + steps:
            widget.grid_remove()

    for row_idx, glacier in enumerate(paginated_glaciers, start=2):
        name_label = update_table.labels[glacier]["name"]
        name_label.grid(row=row_idx, column=0)

        for col_idx, step in enumerate(steps, start=1):
            lbl = update_table.labels[glacier][step]
            lbl.grid(row=row_idx, column=col_idx)

    canvas.configure(scrollregion=canvas.bbox("all"))

    prev_button.config(state=tk.NORMAL if page > 0 else tk.DISABLED)
    next_button.config(state=tk.NORMAL if end_idx < len(glaciers) else tk.DISABLED)

def update_table(key, filter_text=""):
    filtered_glaciers = [glacier for glacier in update_table.glaciers if filter_text.lower() in glacier.lower()]
    sorted_glaciers = sorted(filtered_glaciers, key=lambda g: [check_files_present(g, step, selected_key.get()) for step in update_table.steps], reverse=True)
    update_table.sorted_glaciers = sorted_glaciers
    update_table_page(0)

def update_button_states():
    for step in update_table.steps:
        btn = update_table.buttons[step]
        if selected_glaciers.get(step):
            btn.config(state=tk.NORMAL)
        else:
            btn.config(state=tk.DISABLED)

def toggle_glacier_selection(glacier, step):
    if step not in selected_glaciers:
        selected_glaciers[step] = set()

    if glacier in selected_glaciers[step]:
        selected_glaciers[step].remove(glacier)
    else:
        selected_glaciers[step].add(glacier)

    lbl = update_table.labels[glacier][step]
    bg_color = 'yellow' if glacier in selected_glaciers[step] else 'red'
    lbl.config(bg=bg_color)

    update_button_states()

def filter_glaciers(event):
    if selected_key := key_dropdown.get():
        filter_text = search_var.get()
        update_table(selected_key, filter_text)

root = tk.Tk()
root.title("Glacier Experiment Tracker")

search_var = tk.StringVar()
search_entry = tk.Entry(root, textvariable=search_var)
search_entry.pack()
search_entry.bind('<Return>', filter_glaciers)

selected_key = tk.StringVar()

key_options = list_keys()
key_dropdown = ttk.Combobox(root, textvariable=selected_key, values=key_options)
key_dropdown.pack()
key_dropdown.bind("<<ComboboxSelected>>", on_key_selected)

canvas = tk.Canvas(root)
canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)

scrollbar = ttk.Scrollbar(root, orient=tk.VERTICAL, command=canvas.yview)
scrollbar.pack(side=tk.RIGHT, fill=tk.Y)

canvas.configure(yscrollcommand=scrollbar.set)

table_frame = ttk.Frame(canvas)
table_frame_id = canvas.create_window((0, 0), window=table_frame, anchor="nw")

def on_frame_configure(event):
    canvas.configure(scrollregion=canvas.bbox("all"))

table_frame.bind("<Configure>", on_frame_configure)

prev_button = tk.Button(root, text="Previous", command=lambda: update_table_page(CURRENT_PAGE - 1))
prev_button.pack(side=tk.LEFT, padx=10, pady=10)
next_button = tk.Button(root, text="Next", command=lambda: update_table_page(CURRENT_PAGE + 1))
next_button.pack(side=tk.RIGHT, padx=10, pady=10)

root.mainloop()
