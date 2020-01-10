# Doctor-Patient-Networks

<p align="center">
  <img src="./Images/Figure.png" height="500">
</p>

# Overview
Using private insurance data, this repository tracks patient-doctor ties by opioid prescriptions, and estimates how a patient's (n = ~150m patient-quarters) position in the network may predict subsequent opioid abuse. Based on prior research on the topic, I assume that patients who are more central in the patient-doctor network are likely intentionally seeking generous providers (i.e. they are doctor shopping), and I therefore expect that high node-ranks will be associated with subsequent opioid abuse. The central goal of this project, however, is to determine how to best measure centrality in bipartite networks. The majority of prior work measures centrality in large bipartite social networks based on a one-mode projection of the network with the PageRank algorithm, but this method loses important information about the topology of bipartite networks. Instead, this repository measures centrality with a series of bipartite ranking algorithms, and compares their performance by comparing their ability to predict subsequent opioid overdose across a variety of models. Results indicate that bipartite measures of centrality/doctorshopping in doctor-patient networks (especially BGRM and BiRank) are consistently more predictive of subsequent opioid abuse than are unipartite measures of centrality (PageRank). These findings suggest that BGRM and BiRank are preferable to PageRank in measuring centrality in real-world bipartite social networks. 

