import glob
import os

num = 10000
output_dir = "./First " + str(num) + " words"

print("\nSTART")
print("EDITING FILES TO ~"+ str(num) + " WORDS")

files = glob.glob( "./To_do/*.txt" )
##print(files)

if not os.path.exists(output_dir):
    os.mkdir(output_dir)


for file in files:
    filename = file[8:]
    print(filename)

    opened_file = open( file, "r", encoding="utf-8" )
    text = opened_file.read()
    opened_file.close()

    words = text.split()
    first_n_words = words[0:num]

    ##print("\nFIRST " + str(num) + " WORDS")
    ##print(first_n_words)

    while first_n_words[-1][-1] != ".":
        first_n_words.pop()
    
    ##print("\nEDIT")
    ##print(first_n_words)

    new_text = " ".join(first_n_words)
    ##print(new_text)

    write_file = open(output_dir + "/" + filename, "w", encoding="utf-8")
    write_file.write(new_text)
    write_file.close()
    