#!/usr/bin/env python
# -*- coding: utf-8 -*-
#Functions for crowdflower main 
#read files

# Read kaggle files and convert each string column into a string of strings anr remove stop words
import csv
def unicode_csv_reader(utf8_data, dialect=csv.excel, **kwargs):
	    csv_reader = csv.reader(utf8_data, dialect=dialect, **kwargs)
            for row in csv_reader:
			        yield [unicode(cell, 'utf-8') for cell in row]

def ReadFiles(query,title,descr,median,var,queryt,titlet,descrt):
	import csv,sys
	from nltk.tokenize import word_tokenize
	reload(sys)
        sys.setdefaultencoding("utf-8")
	query_aux=[]
	title_aux=[]
	descr_aux=[]
	trainReader=unicode_csv_reader(open('../data/train.csv','rb'))
	train=list(trainReader)
	for n in range(1,len(train)):    
	    # we compute the non tokenize statistics while reading
	    query_aux=word_tokenize(train[n][1])  #separate sentence in words
	    query_aux = map(lambda x: x.lower(), query_aux) #put all in lowercase letters
            RemoveWords(query_aux)  # remove common words (is, a, the,...)
            query.append(query_aux) 
	    title_aux=word_tokenize(train[n][2])
	    title_aux = map(lambda x: x.lower(), title_aux)
            RemoveWords(title_aux)
	    title.append(title_aux) 
	    if train[n][3]=="":  # if no descriptio use title stats
	         descr_aux=word_tokenize(train[n][2]) 
            else:
	         descr_aux=word_tokenize(train[n][3]) 
	    descr_aux = map(lambda x: x.lower(), descr_aux)
            RemoveWords(descr_aux)
	    descr.append(descr_aux) 
	    median.append(int(train[n][4])-1)  # classes should start on 0
	    var.append(train[n][5])
        queryt_aux=[]
        titlet_aux=[]
        descrt_aux=[]
         
	#f=open('../data/test.csv','rb')
	testReader=unicode_csv_reader(open('../data/test.csv','rb'))
	test=list(testReader)
	for n in range(1,len(test)):    
	    queryt_aux=word_tokenize(test[n][1]) 
	    queryt_aux = map(lambda x: x.lower(), queryt_aux)
	    RemoveWords(queryt_aux)
	    queryt.append(queryt_aux) 
	    titlet_aux=word_tokenize(test[n][2])
	    titlet_aux = map(lambda x: x.lower(), titlet_aux)
            RemoveWords(titlet_aux)
	    titlet.append(titlet_aux) 
	    if (test[n][3]==""):  # if desctiption not there use title stats
		 descrt_aux=word_tokenize(test[n][2])
	    else:
	         descrt_aux=word_tokenize(test[n][3]) 
	    descrt_aux = map(lambda x: x.lower(), descrt_aux)
            RemoveWords(descrt_aux)
	    descrt.append(descrt_aux) 
	return query,descr,median,var,queryt,descrt,titlet

# Remove Stop words (a,the,is, have), from a list of words
def RemoveWords(lwords):
     from nltk.corpus import names, stopwords
     words=stopwords.words('english') 	
     for w in lwords:
         try:
            words.index(w)
	    lwords.remove(w) 
         except ValueError: 
	    lwords=lwords
     return lwords

def FindExactSequence(a,b,c):
	import re
        global ExST,ExSD
        a=' '.join(a)
        b=' '.join(b)
        c=' '.join(c)
        coin=re.findall(a,b)
        ExST=len(coin)  # how many times the query is found
	coin=re.findall(a,c)
        ExSD=len(coin)  # how many times the query is found
def FindSimilarSequence(a,b,c):
        import difflib
        global SimST,SimSD
        a=' '.join(a)
        b=' '.join(b)
        c=' '.join(c)
	seq=difflib.SequenceMatcher(a=a, b=b)  # ja està tot en minuscules
        SimST=seq.ratio()
	seq=difflib.SequenceMatcher(a=a, b=c[0:140]) # Si esta en los primeros 14444haracters
        SimSD=seq.ratio()
# Find number of words and aceptions in the query, equal words and equal sentence and how many nouns match
def FindIntersect(list1,list21,list22,listN1,listN2,lista,listb,listNa,listNl,S1,S2,SimT,SimD):
     # List1, List21 and list22 are lists of lists. We compare here list1 with list22 and list21
     from nltk.corpus import wordnet as wn
     global Na, Nl,ExST,ExSD,SimST,SimSD
     for n in range(len(list1)):
          QueryStats(list1[n])
	  listNl.append(Nl)
	  listNa.append(Na)
	  list_out=[]
          list_out = list(set(list1[n]) & set(list21[n]))
	  cont=0
	  for nn in range(len(list_out)):
              if len(wn.synsets(list_out[nn]))!=0 and (wn.synsets(list_out[nn])[0]).pos()=='n':   # si coincide en el nombre
		 cont+=1
	  lista.append(cont)   	 
	  listN1.append(float(len(list_out))/float(len(list1[n])))
	  list_out = list(set(list1[n]) & set(list22[n]))
          listN2.append(float(len(list_out))/float(len(list1[n])))
	  cont2=0
	  for nn in range(len(list_out)):
              if len(wn.synsets(list_out[nn]))!=0 and (wn.synsets(list_out[nn])[0]).pos()=='n':   # si coincide en el nombre
		 cont2+=1
	  listb.append(cont2)   	 
	  FindExactSequence(list1[n],list21[n],list22[n])
	  S1.append(ExST)
	  S2.append(ExSD)
	  FindSimilarSequence(list1[n],list21[n],list22[n])
	  SimT.append(SimST)
	  SimD.append(SimSD)
	  #print 'HOLA SIM', n,SimST,SimSD
     return listN1,listN2,lista,listb,listNa,listNl,S1,S2,SimT,SimD

## This functions before tokenize

# Query properties. N Aception
def QueryStats(a):
      global Na,Nl
      from nltk.corpus import wordnet as wn 
      # numer of words in the query
      Na=0
      Nl=len(a)
      for i in range(Nl):
	  Na+=len(wn.synsets(a[i])) 
      Na=float(Na)/float(Nl)

#Find Similarity and category shared among words as defined in Wordnet.

def Similarity(list1,list2,list3,CatQT,SimQT,CatQD,SimQD):

        from nltk.corpus import sentiwordnet as swn
        from nltk.corpus import wordnet as wn
        from nltk import pos_tag
	from nltk.corpus import wordnet_ic

        import numpy as np
	N=len(list1)
	from nltk.corpus import gutenberg
	gut_ic = wn.ic(gutenberg, False, 0.0)
	for n in range(len(list1)):
		if n%100 == 0:
		   print 'SEARCH', n
		list11=list1[n]
		list22=list2[n]
		list33=list3[n]
		#itendify if the semantics
		l1_tagged=pos_tag(list11)
		l2_tagged=pos_tag(list22)
		l3_tagged=pos_tag(list33)
                # declare counters and scores
		contQ=contT=contD=contST=contSD=contSimT=contSimD=contCT=contCD=0  # counts query,title,decr words and pairs
		score12=score13=0
		cat12=cat13=0      # shared category of words 


                 # AL PRINCIPIO VAMOS A BUSCAR SIMILITUDES ENTRE LA PRIMERA ACEPCIoN DE LA PALABRA [0]. 
		 #MIRAMOS NOMBRES (NN), ADJETIVOS (JJ) y VERBOS (VB,VBP and VBG) y adverbios (RB), 
		 #evitando preposiciones, verbos como can, is, will, ...;
                #print 'QUERY',list11
		for i1,w1 in enumerate(list11):
		    if len(wn.synsets(w1))!=0 and ('NN' in l1_tagged[i1][1] or 'VB' in l1_tagged[i1][1] or 'JJ' in l1_tagged[i1][1] or 'RB' in l1_tagged[i1][1]):		
	                w1s=wn.synsets(w1)[0]
	                #print 'QUERY ACCEPTED',n,i1,w1,l1_tagged[i1][1] 
			#if len(swn.senti_synsets(w1))!=0:  #sentimental score not really helpful
			    #posScore1+=swn.senti_synsets(w1)[0].pos_score()
			    #negScore1+=swn.senti_synsets(w1)[0].neg_score()
			    #contQ+=1
		        
			for i2,w2 in enumerate(list22): 
	                     if len(wn.synsets(w2))!=0 and ('NN' in l2_tagged[i2][1] or 'VB' in l2_tagged[i2][1] or 'JJ' in l2_tagged[i2][1] or 'RB' in l2_tagged[i2][1]):		
			            #print 'TITLE ACCEPTED',n,i1,i2,w2,l2_tagged[i2][1]
			            contT+=1
				    w2s=wn.synsets(w2)[0]
				    if w2s.path_similarity(w1s)!=None:
				        contSimT+=1
					score12+=w2s.path_similarity(w1s) #estimated similarity
				    if len(w2s.lowest_common_hypernyms(w1s))!=0:
				        contCT+=1
				        cat12+=w1s.lowest_common_hypernyms(w2s)[0].min_depth() #lowest category 
					#shared (e.g. domestic animal, vertebrate,...different category == 0
		        
			for i3,w3 in enumerate(list33): 
			     #if len(wn.synsets(w3))!=0 and (l1_tagged[i1][1] in l3_tagged[i3][1] or 
			     #l3_tagged[i3][1] in l1_tagged[i1][1]):
	                     if len(wn.synsets(w3))!=0 and ('NN' in l3_tagged[i3][1] or 'VB' in l3_tagged[i3][1] or 'JJ' in l3_tagged[i3][1] or 'RB' in l3_tagged[i3][1]):		
			            w3s=wn.synsets(w3)[0]
				    contD+=1
				    #print 'DESC ACCEPTED',n,w3,w3s,w1s,l3_tagged[i3][1],w3s.path_similarity(w1s) 
				    if w3s.path_similarity(w1s)!=None:
			                 contSimD+=1
				         score13+=w3s.path_similarity(w1s) #estimated similarity
				    #print scoreJcn13,scoreRes13,scoreLin13,scoreWup13,scoreLch13
				    if len(w3s.lowest_common_hypernyms(w1s))!=0:
				         contCD+=1
					 cat13+=w3s.lowest_common_hypernyms(w1s)[0].min_depth() #lowest category
					 #shared (e.g. domestic animal, vertebrate,...different category == 0
## se esta volviendo loco!!!! REPASAR
		if contCT!=0:     
                     CatQT[n]=(cat12/contCT)
		if contSimT!=0:     
                     SimQT[n]=(score12/contSimT)
	        if contCD!=0:     
                     CatQD[n]=(cat13/contCD)
		if contSimD!=0:     
                     SimQD[n]=(score13/contSimD)
        return CatQT,SimQT,CatQD,SimQD


# Find Exact sequence
