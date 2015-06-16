#!/usr/bin/env python
# -*- coding: utf-8 -*-

# main code
# Read files
# Call functions

# ANTES DE LOS ESTADISTICOS:
  ##  separamos frases en palabras (word_tokenize)
  ##  lo pasamos todo a minusculas  (lower)
  ##  quitamos palabras comunes (stopwords)
  ##  si descripcion no tiene nada usar titulo

# COSAS QUE SE PUEDEN HACER. Añadir tablas AINA, THESAURUS BUSCAR
# AÑADIR TODAS LAS ACEPCIONES, HASTA AHORA SOLO USO LA PRIMERA. CREO QUE ES LA DEFINICION MAS COMUN (ver Dog)
# De momento comparo nombres con verbos porque pos_tag no siempre posiciona bien, y me puede sesgar. 
# FALTAN PALABRAS TECONOLOGICAS COMO USB
# NO IDENTIFICA MARCAS (se podria solucionar con los textos?)
# Similarity of sentences only counting first 140 in the description, if there are many words is =0 even if there is the exact sentence, if it is too far we concider that is a worst search.


import utils
import nltk
import numpy as np
import sys
reload(sys)
sys.setdefaultencoding("utf-8")

query,title,descr,median,var= ([] for i in range(5))
queryt,titlet,descrt= ([] for i in range(3))

#Read Files tokenize and remove non significant words. 
utils.ReadFiles(query,title,descr,median,var,queryt,titlet,descrt)

# First 2 statistics. Find number of exact same words between query and title NQT and query and description NQD for each pair. Nn number of matching nouns, NaQ number of aceptions of query words, NlQ--> length query.
# Also compute Exact sentence match SQT and SQD, adn Similar sentence match SimQT*,SimQD*

NnT,NnD,NaQ,NlQ,NQTtrain,NQDtrain=([] for i in range(6))
SQTtrain,SQDtrain,SimQTtrain,SimQDtrain=([] for i in range(4))

utils.FindIntersect(query,title,descr,NQTtrain,NQDtrain,NnT,NnD,NaQ,NlQ,SQTtrain,SQDtrain,SimQTtrain,SimQDtrain)

SQTtest,SQDtest,SimQTtest,SimQDtest=([] for i in range(4))
NnTt,NnDt,NaQt,NlQt,NQTtest,NQDtest=([] for i in range(6))

utils.FindIntersect(queryt,titlet,descrt,NQTtest,NQDtest,NnTt,NnDt,NaQt,NlQt,SQTtest,SQDtest,SimQTtest,SimQDtest)


# Compute average similarity, category and sentimental Positive and negative value between nouns, verbs and adjectives of the query and the title and description. Saved in the list and arrays:
CQT=np.zeros(len(query))
SQT=np.zeros(len(query))
CQD=np.zeros(len(query))
SQD=np.zeros(len(query))

utils.Similarity(query,title,descr,CQT,SQT,CQD,SQD)
f=open('../data/input2_data_train.txt','w')
ft=open('../data/input2_data_test.txt','w')
for i in range(len(query)):
	A=np.array([median[i],NQTtrain[i],NQDtrain[i],NnT[i],NnD[i],NaQ[i],NlQ[i],SQTtrain[i],SQDtrain[i],SimQTtrain[i],SimQDtrain[i],CQT[i],CQD[i],SQT[i],SQD[i]])
	if i<=len(query)*4/5:
	     print >> f, " ".join(map(str,A))
	else:
	     print >> ft, " ".join(map(str,A))
f.close()
ft.close()
CQTt=np.zeros(len(queryt))
CQDt=np.zeros(len(queryt))
SQTt=np.zeros(len(queryt))
SQDt=np.zeros(len(queryt))

utils.Similarity(queryt,titlet,descrt,CQTt,SQTt,CQDt,SQDt)
f2=open('../data/submission2_test.txt','w')
for i in range(len(queryt)):
	A=np.array([NQTtest[i],NQDtest[i],NnTt[i],NnDt[i],NaQt[i],NlQt[i],SQTtest[i],SQDtest[i],SimQTtest[i],SimQDtest[i],CQTt[i],CQDt[i],SQTt[i],SQDt[i]])
	print >> f2, " ".join(map(str,A))

f2.close()
