import os

location = '/Users/andrewpiper/Desktop/Tests/NovelGerman150'; 	# path where the data is kept


def twenty( name,num ):
	path = location;
	f = open(path+name,'r');
	

	name  = name.replace('.txt','');
	
	
	bio = f.read();
	#print bio;
	l = bio.__len__(); # __len__() string length function
	l = l / 20;
	print l;

	i=0;
	count = 1;path= path + str(num);os.system("mkdir "+path);
	path = path + '/' + name;
	o = open(path+'_'+str(count)+'.txt','w');
	
	f.seek(0);
	#sys.exit(0);

	while True:
		i = i+1;
		if i <= l:
			c = f.read(1)
			#c.encode("utf8")
			if not c:
				print "End of file"
				f.close();
				break
			o.write(c);
		else:
			i=0;
			count=count+1;
			
			o.close();
			if count == 21: break;
			print count;
			o = open(path+'_'+str(count)+'.txt','w');
				
	
	f.close();
	return;

num = 01;
os.chdir(location)
for root, dirs, files in os.walk(location):
	for name in files:			#for each file in the list 
            num=num+1;
            twenty(name,num);
            os.system('rm \"'+ name+ '\"');
