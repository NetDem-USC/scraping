from bs4 import BeautifulSoup
file1 = open(file1,'w')
file2 = open(file2,'r')
Text = BeautifulSoup(file2.read(),'html.parser')
file1.write(Text.prettify())
file2.close()
file1.close()


