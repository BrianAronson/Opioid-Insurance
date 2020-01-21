# Doctor-Patient-Networks

<p align="center">
  <img src="./Images/Figure.png" height="500">
</p>

# Overview
This repository compares the performance among bipartite and unipartite measures of node rank in network of providers and patients who are tied by opioid prescriptions from a sample of ~150m patient-quarter observations. It treats patients with high centrality scores as doctor shoppers, and uses this information to predict subsequent opioid abuse. Specifically it compares models (cox proportional hazards, within-person random intercepts, and within-person fixed effects) that predict opioid abuse based on different centrality measures. It finds that bipartite measures of centrality/doctorshopping in doctor-patient networks are consistently more predictive of subsequent opioid abuse than are unipartite measures of centrality (PageRank). Moreover, it finds that BGRM and BiRank typically perform best among bipartite measures of centrality.

# Theoretical Background 
Recent research suggests that a patient’s PageRank centrality in doctor-patient networks may be a strong indicator of a doctor shopping. However, as in the great majority studies about bipartite networks, prior work measures PageRank centrality based on a one-mode projection of doctor-patient ties. Such methods lose important information about the topology of bipartite networks. This repository tests alternative measures of centrality in bipartite networks. More generally, this research demonstrates that bipartite measures of node rank can substantially improve our ability to measure centrality in real-world bipartite social networks.

# Usage
This repository was built only for personal use, so it requires a few steps for migrating to a new computer:
- Attain access to the private insurance dataset.
- Install all required libraries used in the repository.

Afterwards, users are advised to run all scripts within the "Code" directory in the order of their numerical prefixes. Scripts without a numerical prefix should be run after scripts with numerical prefixes are run. Many supplementary scripts for visualizing the data and for running models on larger samples of the data are in the Archived folder. These files are considered non-essential for this paper, but were useful in earlier points of this project and are outlined below.

# Details
Below is a brief overview of each script by folder. For privacy reasons, scripts for renaming the baseline data are omitted from this repository.

#### Code/
    - **1) AWS - Prepare dataset.R**
        - Pull some patient-level variables and corresponding patient-doctor edge lists for each year-quarter of interest from S3. 
        - Estimate stats about the patient-doctor edge lists
        - Mean-center centrality estimates by component size.
    - **2) HPC - Download dataset.R**
        - Download the data onto a secure HPC.
    - **3) HPC - Clean dataset.R**
        - Rename variables, create a few new ones, create transformed variables (logged, 99th percentile, standardized, lagged),
    - **4) HPC - Prepare models.R**
        - A create a dataframe containing hundreds of model variants of interest.
    - **5) HPC - Estimate models.R**
        - Estimate each model in parallel and format results.
    - **6) HPC - Format results.R**
        - Format and graph key parameter estimates

#### Archived
    - **X) AWS - Big data - Run Models.R**
        - Run models using memory-friendly functions (ff and bigglm.ffdf).
    - **X) Big data functions from obsolete files.R**
        - Personal memory-friendly functions for pulling out model predictions, calculating AUC, and computing marginal effects, 
    - **X) HPC - Create mean - variance variables.R**
        - Approach for creating within person mean-variance transformations on big data without ff.df.

#### Archived/Visualizations
    - **X) HPC - Plot pagerank by component size and IO in big dataset.R**
    - **x01) Fig - Correlation graph - vars by biranks.R**
        - Illustrates how rank estimates vary in their correlation to other indicators of doctor shopping.
    - **x02) Fig - Line graph - biranks by component size.R**
    - **x03) Fig - Line graph - biranks by pagerank.R**
    - **x04) Fig - Facet line graphs - multiple vars by biranks.R**
    - **X05) Fig - Conceptual sociograms.R**
    - **x06) Tab - Component size by N.R**
    - **x07) Fig - Density plots - rank variables.R**
        - Essential script for helping me determine outliers in the distribution of most rank variables. It turns out that many spikes are from networks with tiny components.
    - **x08) Fig - Density Heatmap - birank by pagerank density.R**
        - Illustrate the direct association between all values of BiRank and PageRank
    - **x09) Fig - Density Heatmap - birank by pagerank.R**
        - Compare how well BiRank and PageRank are associated with various indicators of opioid abuse.
    - **x10) Fig - US map - county by mean birank.R**
        - Generate a heatmap of mean rank score across the U.S. Illustrates both the high coverage of the sample and network endogeneity.
    - **x11) Fig - Appalachian map with state boundaries - county by opioid abuse.R**
        - Generate a map of counties used in a smaller sample of the data.