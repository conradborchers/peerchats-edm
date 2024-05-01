# Instructional Factors Analysis of Peer Tutoring Chat Messages

Supplementary repository of the full paper "Understanding Learning in Collaborative Tutoring Systems via Instructional Factors Analysis of Peer Tutoring Chats" accepted at EDM '24.

Citation:

Borchers, C., Yang, K., Lin, J., Rummel, N., Koedinger, K. R., & Aleven, V. (2024). Learning Rates in Collaborative Tutoring Systems by Peer Tutoring Chat Types. In *Proceedings of the 17th International Conference on Educational Data Mining (EDM)*. Atlanta, GA, USA.

```
@inproceedings{borchers2024learning,
  title={Learning Rates in Collaborative Tutoring Systems by Peer Tutoring Chat Types},
  author={Borchers, Conrad and Yang, Kexin and Lin, Jionghao and Rummel, Nikol and Koedinger, Kenneth R. and Aleven, Vincent},
  booktitle={Proceedings of the 17th International Conference on Educational Data Mining},
  year={2024}
}
```

## Folder structure

* `1-chat-classification.ipynb`: Notebook to reproduce the BERT model training procedure to classify student chat messages, including applying the classifier to the full log data set featured in this study.

* `2-analysis.R`: R script to reproduce all analyses featured in the manuscript from the output generated in `1-chat-classification.ipynb`.

## Data availability and setup

First, open the three CMU Datashop dataset webpages in your browser.

https://pslcdatashop.web.cmu.edu/DatasetInfo?datasetId=5153
<br>https://pslcdatashop.web.cmu.edu/DatasetInfo?datasetId=5549
<br>https://pslcdatashop.web.cmu.edu/DatasetInfo?datasetId=5604

To access the datasets, you will need to request access with an account on DataShop. You can create an account using your Google or GitHub account, whichever is easiest.

Once you have created an account, navigate back to the dataset webpages and click on the button `Request Access`. Provide your reasoning to access the dataset and click `Confirm`. You should receive an email once the PI approves the request; however, you can also check by seeing whether you can click the `Export` button on the project webpage.

To get the final datasets, click the `Export` button. On the left hand side, make sure under `Shared Samples` that there is a checkbox next to `All Data` by clicking it. Then, click the `Export Transactions` button when it appears. Wait for the server to process your request, and then you should have three files `ds*_tx_All_Data_*_<timestamp>.txt`.

You should then rename the three files to 

`data/logdataschool{1,2,3}.txt`

to get started. The specific order of these three files does not matter.

