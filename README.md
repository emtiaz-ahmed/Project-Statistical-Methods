# Project-Statistical-Methods

## Task 1: Quality Control ##
**1. Biographic Data:** Draw the barplot for gender distribution; histogram for age distribution. </br></br>
**2. Trait Psychometric Data:** Draw the histogram for TAI scores. Please consider only the total TAI score, which can be found at the *tp.csv file residing at the root of each subject file system. TAI takes values in the range 20-80, with scores up to low 40s considered normal, while higher scores considered indicative of overanxious individuals.</br></br>
**3. State Psychometric Data:** For each subject draw the barplots for all the NASA-TLX subscales per task. This will give two figures per subject per subscale, one for suturing and one for cutting, where the evolution of the scores from the initial to the final session will be evident. There should be a downward trend, reflecting increased facility with the tasks. Keep in mind that NASA-TLX subscales are scored in the range 1-20.</br></br>
**4. Perinasal Perspiration (Stress) Signal Data:** For each session of each subject draw the stress signals, using black for baseline, green for cutting, and red for suturing. Generally speaking, the baseline signal should be at a lower level than the other two. In total, you will draw five figures for each subject or whatever the number of his/her sessions is. It is worth noting that the sampling rate fluctuates around 7 frames per second. We suggest that you downsample to 1 frame per second and use these downsampled signals to perform statistics. This averaging will give you a smoother signal without affecting the validity of the data, as arousal responses manifest in 2-4 seconds, that is, at a resolution higher than the averaging that we suggest.</br></br>
**5. Performance Data:** Draw the time barplots for each subject, using different colors for each task (cutting vs. suturing). You should observe a downward trend for the cutting task. Draw also the accuracy barplots for each subject, using different colors for each task (cutting vs. suturing). You should observe an ascending trend. Please note that there are two accuracy variables: one provides an overall accuracy score, while the other indicates the number of sutures properly completed in the allotted time. The allotted time was 20 minutes for the suturing task, while it was task-dependent for the cutting task.</br></br>

**IMPORTANT NOTE** wrt Performance Data We have uploaded new performance data, with excplicit accuracy scores per task. Hence, the cutting task has its own accuracy scores and so is the case with the sutruting task. Please use these accuracy scores instead of the legacy generic accuracy score. Obviously, these more specific scores will enable more specific and sound inferences.

**HYPOTHESES - TESTING - INTERPRETATION** You need to formulate a set of pre-planned hypotheses that you would like to test. Such a set should be driven by the experimental design. This is a micro-surgical education study, with two distinct tasks that present different challenge levels. Some key questions are if the levels of stress and the type of task played any role in the accuracy scores, and if the accuracy scores changed over time. As I explained in task, you can formulate a linear model to test this set of hypotheses. The results of these tests can provide revealing insight. For example, if you find that there was no accuracy improvement over time in the suturing task, this means that the educational process was a failure in this respect. A critical key of any analysis, and what will be weighed heavily in this project, is the articulation of insightful interpretations to the results of the tests.

**CAUTION:** here are a host of things that you need to be careful when you employ linear modeling here. For exmaple, you need to check for normality.

Please also note that you can (and should) formulate more hypotheses and apply appropriate methods to test them. You may need to do so anyway, for researching deeper some of the linear modeling results.

**EXEMPLARY ANALYSIS** An exemplary analysis of laparoscopic surgical education has been published by Pavlidis et al. here: https://www.nature.com/articles/srep00305

Although there are many differences with the current microsurgery project, still the two studies are in the broader area of surgical education, and at the very least you can see in that SREP paper how to properly construct statistical figures and articulate conclusions based on the results of statistical tests.

**Units**:
</br>
Gender: Male=1, Female=2 </br>
Cutting time: 10 </br>
Suturing time: 20 </br>
Sutures: 0-6 </br>
Score: 6-30 </br>

