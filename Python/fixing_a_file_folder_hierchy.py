import pandas as pd
import os
import re

direc = "T:\\folder"
folders = [folder for folder in os.listdir(direc) if re.search(".*app_name.*",folder)]

def file_merger(keyword,save_dir):
    df = pd.DataFrame()
    os.chdir(direc)
    print(direc)
    for fold in folders:
        os.chdir(os.path.join(direc,fold))
        print("\nWorking on: ", os.getcwd())
        file_to_read = [x for x in os.listdir(os.getcwd()) if re.match(".*_"+keyword+".*",x)]
        file_to_read = "".join(str(x) for x in file_to_read) #turns list to str
        print("\Appending : ", os.getcwd()+file_to_read)
        temp = pd.read_csv(file_to_read, index_col = False)
        df = df.append(temp)
    df.to_csv(save_dir+'\\'+keyword+'_complete.csv', index = False)

file_merger("Sent","T:\\folder\\all_combined_files")
file_merger("Opens","T:\\folder\\all_combined_files")
file_merger("Clicks","T:\\folder\\all_combined_files")
file_merger("Complaints","T:\\folder\\all_combined_files")
file_merger("Unsubs","T:\\folder\\all_combined_files")
file_merger("Bounces","T:\\folder\\all_combined_files")
file_merger("SendJobs","T:\\folder\\all_combined_files")
        

  
