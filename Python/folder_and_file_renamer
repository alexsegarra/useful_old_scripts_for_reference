import os
import re
import sys

direc = "T:\InterTeam\Tableau\Marketing\Sources\cce_sfmc_data"
folders = [folder for folder in os.listdir(direc) if re.search(".*seamless.*",folder)]
print(folders)
txt_to_kill="all_the_things_seamless_"
for fold in folders:
    os.chdir(os.path.join(direc,fold))
    print("\nWorking on: ", os.getcwd())
    new_text = re.sub(txt_to_kill,"", fold)
    print("\nFolder Text to be appended:",fold)
    for files in os.listdir(os.getcwd()):
        src = os.getcwd() +'\\'+ files
        dst = os.getcwd() +'\\'+new_text+"_"+files
        print("\nChanging ",src," to ",dst)
        os.rename(src,dst)
        
#EOF
    
          
