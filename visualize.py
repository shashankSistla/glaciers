import tkinter as tk
from tkinter import ttk
from PIL import Image, ImageTk, ImageSequence
import os

KEYS_DIR = "keys"
OUTPUT_DIR = "output"

STEPS = { # This dictionary contains the file paths and the resize scales
    "Flowline": ("02_GD_flowline/output/{glacier_name}_flowline_dem.png",100),
    "Intensity Profiles": ("03_extract_IP/output/plots/{glacier_name}_ndsi_intensityprofile.png",30),
    "Candidate Paths": ("04_candidate_paths/output/plots/{glacier_name}_candidate_paths.png",100),
    "Clustering - Reachability Plot": ("05_clustering/output/plots/{glacier_name}_reachability.png",100),
    "Clustering - Clustered Paths": ("05_clustering/output/plots/{glacier_name}_clustered.png",100),
    "Smoothened": ("06_smoothing/output/plots/{glacier_name}_smoothened.png",100),
    "Regression": ("07_fda/output/plots/{glacier_name}_regression.png", 100),
    "Landsat Gif": ("08_make_gifs/output/{glacier_name}.gif", 100)
}

def list_keys():
    return [f[:-2] for f in os.listdir(KEYS_DIR) if f.endswith('.R')]

def list_glaciers(key):
    key_dir = os.path.join(OUTPUT_DIR, key, "01_prepare_dem", "output")
    if not os.path.exists(key_dir):
        return []
    glaciers = [f.split('_dem.tif')[0] for f in os.listdir(key_dir) if f.endswith('_dem.tif')]
    return glaciers

def load_images(glacier, steps, key):
    for widget in image_frame.winfo_children():
        widget.destroy()

    column_count = 2  # Number of columns to display images
    column = 0
    row = 0

    for step in steps:
        if step in STEPS:
            image_path, scale_percent = STEPS[step]
            image_path = os.path.join(OUTPUT_DIR, key, image_path.format(glacier_name=glacier))
            if os.path.exists(image_path):
                if image_path.endswith('.gif'):
                    img = Image.open(image_path)
                    frames = [ImageTk.PhotoImage(frame.copy().resize((int(frame.width * scale_percent / 100), int(frame.height * scale_percent / 100)), Image.ANTIALIAS)) for frame in ImageSequence.Iterator(img)]
                    lbl = tk.Label(image_frame)
                    lbl.frames = frames
                    lbl.delay = img.info.get('duration', 100)
                    lbl.grid(row=row, column=column, padx=5, pady=5)
                    animate_gif(lbl, 0)
                else:
                    img = Image.open(image_path)
                    width, height = img.size
                    new_width = int(width * scale_percent / 100)
                    new_height = int(height * scale_percent / 100)
                    img = img.resize((new_width, new_height), Image.ANTIALIAS)
                    img = ImageTk.PhotoImage(img)
                    lbl = tk.Label(image_frame, image=img)
                    lbl.image = img  # Keep a reference to avoid garbage collection
                    lbl.grid(row=row, column=column, padx=5, pady=5)
                column += 1
                if column >= column_count:
                    column = 0
                    row += 1

def animate_gif(label, frame_index):
    frame = label.frames[frame_index]
    label.config(image=frame)
    frame_index = (frame_index + 1) % len(label.frames)
    label.after(label.delay, animate_gif, label, frame_index)

def on_key_selected(event):
    selected_key = key_dropdown.get()
    glaciers = list_glaciers(selected_key)
    glacier_dropdown['values'] = glaciers
    if glaciers:
        glacier_dropdown.current(0)
    update_images()
    update_step_checkboxes()

def on_glacier_selected(event):
    selected_glacier.set(glacier_dropdown.get())
    update_images()
    update_step_checkboxes()

def update_images():
    key = key_dropdown.get()
    glacier = glacier_dropdown.get()
    selected_steps = [step for step, var in step_vars.items() if var.get()]
    load_images(glacier, selected_steps, key)

def update_step_checkboxes():
    key = key_dropdown.get()
    glacier = glacier_dropdown.get()
    for step, (path, _) in STEPS.items():
        image_path = os.path.join(OUTPUT_DIR, key, path.format(glacier_name=glacier))
        if os.path.exists(image_path):
            step_checkbuttons[step].config(state=tk.NORMAL)
        else:
            step_checkbuttons[step].config(state=tk.DISABLED)

def filter_glaciers(event):
    filter_text = search_var.get().lower()
    all_glaciers = list_glaciers(selected_key.get())
    filtered_glaciers = [glacier for glacier in all_glaciers if filter_text in glacier.lower()]
    glacier_dropdown['values'] = filtered_glaciers
    if filtered_glaciers:
        glacier_dropdown.current(0)
    update_images()
    update_step_checkboxes()

def prev_glacier():
    current_index = glacier_dropdown.current()
    if current_index > 0:
        glacier_dropdown.current(current_index - 1)
        update_images()
        update_step_checkboxes()

def next_glacier():
    current_index = glacier_dropdown.current()
    if current_index < len(glacier_dropdown['values']) - 1:
        glacier_dropdown.current(current_index + 1)
        update_images()
        update_step_checkboxes()

root = tk.Tk()
root.title("Glacier Result Viewer")

selected_key = tk.StringVar()
key_dropdown = ttk.Combobox(root, textvariable=selected_key, values=list_keys())
key_dropdown.pack()
key_dropdown.bind("<<ComboboxSelected>>", on_key_selected)

search_var = tk.StringVar()
search_entry = tk.Entry(root, textvariable=search_var)
search_entry.pack()
search_entry.bind('<KeyRelease>', filter_glaciers)

nav_frame = ttk.Frame(root)
nav_frame.pack()

prev_button = ttk.Button(nav_frame, text="Prev", command=prev_glacier)
prev_button.pack(side=tk.LEFT)

selected_glacier = tk.StringVar()
glacier_dropdown = ttk.Combobox(nav_frame, textvariable=selected_glacier)
glacier_dropdown.pack(side=tk.LEFT)
glacier_dropdown.bind("<<ComboboxSelected>>", on_glacier_selected)

next_button = ttk.Button(nav_frame, text="Next", command=next_glacier)
next_button.pack(side=tk.LEFT)

step_vars = {}
step_checkbuttons = {}
step_frame = ttk.Frame(root)
step_frame.pack()

for step in STEPS.keys():
    var = tk.BooleanVar()
    chk = ttk.Checkbutton(step_frame, text=step, variable=var, command=update_images)
    chk.pack(side=tk.LEFT)
    step_vars[step] = var
    step_checkbuttons[step] = chk

canvas = tk.Canvas(root)
canvas.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)

scrollbar_y = ttk.Scrollbar(root, orient=tk.VERTICAL, command=canvas.yview)
scrollbar_y.pack(side=tk.RIGHT, fill=tk.Y)

scrollbar_x = ttk.Scrollbar(root, orient=tk.HORIZONTAL, command=canvas.xview)
scrollbar_x.pack(side=tk.BOTTOM, fill=tk.X)

canvas.configure(yscrollcommand=scrollbar_y.set, xscrollcommand=scrollbar_x.set)

image_frame = ttk.Frame(canvas)
image_frame_id = canvas.create_window((0, 0), window=image_frame, anchor="nw")

def on_frame_configure(event):
    canvas.configure(scrollregion=canvas.bbox("all"))

image_frame.bind("<Configure>", on_frame_configure)

root.mainloop()
