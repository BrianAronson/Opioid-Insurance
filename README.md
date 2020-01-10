# Doctor-Patient-Networks

<p align="center">
  <img src="./Images/Figure.png" height="500">
</p>

# Overview
This repository compares the performance among bipartite and unipartite measures of node rank in network of providers and patients who are tied by opioid prescriptions from a sample of ~150m patient-quarter observations. It treats patients with high centrality scores as doctor shoppers, and uses this information to predict subsequent opioid abuse. Specifically it compares models (cox proportional hazards, within-person random intercepts, and within-person fixed effects) that predict opioid abuse based on different centrality measures. It finds that bipartite measures of centrality/doctorshopping in doctor-patient networks are consistently more predictive of subsequent opioid abuse than are unipartite measures of centrality (PageRank). Moreover, it finds that BGRM and BiRank typically perform best among bipartite measures of centrality.

# Theoretical Background 
Recent research suggests that a patient’s PageRank centrality in doctor-patient networks may be a strong indicator of a doctor shopping. However, as in the great majority studies about bipartite networks, prior work measures PageRank centrality based on a one-mode projection of doctor-patient ties. Such methods lose important information about the topology of bipartite networks. This repository tests alternative measures of centrality in bipartite networks. More generally, this research demonstrates that bipartite measures of node rank can substantially improve our ability to measure centrality in real-world bipartite social networks.
