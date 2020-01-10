# Doctor-Patient-Networks

<p align="center">
  <img src="./Images/Figure.png" height="500">
</p>

# Overview
Using private insurance data, this repository tracks patient-doctor ties by opioid prescriptions, and estimates how a patient's position in the network may predict subsequent opioid abuse (n = ~150m patient-quarters). Based on prior research on the topic, this repository assumes that patients who are more central in the patient-doctor network are likely intentionally seek generous providers (i.e. be doctor shoppers), and is therefore constructed with the expectation that high node-ranks will be associated with subsequent opioid abuse. The key innovation and goal of this project, however, is to determine how to best measure centrality in bipartite networks. The majority of prior work measures centrality in large bipartite social networks based on a one-mode projection of the network with the PageRank algorithm; however, this method loses important information about the topology of bipartite networks. Instead, this repository measures centrality with a series of bipartite ranking algorithms and benchmarks these algorithms based on their ability to predict subsequent opioid overdose. Results indicate that bipartite measures of centrality/doctorshopping in doctor-patient networks (especially BGRM and BiRank) are consistently more predictive of subsequent opioid abuse than are unipartite measures of centrality (PageRank). These findings imply that BGRM and BiRank are preferable to PageRank in measuring centrality in real-world bipartite social networks. 
