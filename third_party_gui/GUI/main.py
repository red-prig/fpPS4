import tkinter
import tkinter.messagebox
import tkinter.filedialog
import os
import os.path

fpps4 = ""
game = ""
#create open fpps4 dialog
def open_fpps4():
    global fpps4
    file_path = tkinter.filedialog.askopenfilename(title="Select the fpps4.exe", filetypes=[("Applications", "*.exe")])
    if file_path:
        fpps4_path_label['text'] = file_path
        fpps4 = file_path
    else:
        fpps4_path_label['text'] = "No file selected"
#create open game dialog
def open_game():
    global game
    file_path = tkinter.filedialog.askdirectory(title="Select the games Folder")
    if file_path:
        game_path_label['text'] = file_path
        game = file_path
    else:
        game_path_label['text'] = "No file selected"
#autodetect
def autodetect():
    global fpps4
    for dirpath, dirnames, filenames in os.walk("../../"):
        for filename in [f for f in filenames if f.endswith(".exe")]:
            if filename == "fpPS4.exe":
                fpps4 = os.path.abspath(os.path.join(dirpath, filename))
    fpps4_path_label['text'] = fpps4
    tkinter.messagebox.showinfo(title="Found fpPS4!", message=fpps4)
#rungame
def rungame():
    global fpps4
    global game
    if fpps4 == "":
        autodetect()
    if game == "":
        open_game()
    if game != "" and fpps4 != "":
        print(fpps4 + " -e " + game + "/eboot.bin")
        os.system(fpps4 + " -e " + game + "/eboot.bin") 
    else:
        tkinter.messagebox.showinfo(title="Error!", message="Please select a game and the fpPS4 folder!")

#create main window
root = tkinter.Tk()
root.title("fpPS4 GUI")
#open game button
open_game_button = tkinter.Button(root, text="Open game folder", command=open_game)
open_game_button.grid(row=0, column=0)
#open fpps4 button
open_fpps4_button = tkinter.Button(root, text="Open fpps4 file", command=open_fpps4)
open_fpps4_button.grid(row=0, column=2)
#autodetect fpps4 button
open_fpps4_button = tkinter.Button(root, text="Autodedect fpps4", command=autodetect)
open_fpps4_button.grid(row=0, column=4)
#rungame button
open_fpps4_button = tkinter.Button(root, text="Run!", command=rungame)
open_fpps4_button.grid(row=0, column=6)
#print selected game path
game_path_label = tkinter.Label(root, text="No game selected")
game_path_label.grid(row=2, column=0, columnspan=2)
#print selected fpps4 path
fpps4_path_label = tkinter.Label(root, text="fpPS4 not selected")
fpps4_path_label.grid(row=4, column=0, columnspan=2)
#quit
quit_button = tkinter.Button(root, text="Quit", command=root.destroy)
quit_button.grid(row=6, column=6, columnspan=2)
#run main loop
root.mainloop()