---
title: "Measuring Forecasting Proficiency: An Item Response Theory Approach"
author: "<sup>1</sup>Fabio Setti, <sup>1</sup>Leah Feuerstahler, <sup>2,6</sup>Sophie Ma Zhu, <sup>3,6</sup>Nikolay Petrov, <sup>4,6</sup>Ezra Karger, <sup>5,6</sup>Mark Himmelstein"
institute: "<sup>1</sup>Fordham University, <sup>2</sup>University of British Columbia, <sup>3</sup>University of Cambridge, <sup>4</sup>Federal Reserve Bank of Chicago, <sup>5</sup>Georgia Institute of Technology, <sup>6</sup>Forecasting Research Institute"

bibliography: Additional files/R packages.bib
csl: Additional files/apa.csl

title-slide-attributes:
  data-transition: "zoom"
  data-visibility: "uncounted"

format:
  revealjs:
    footer: "SPUDM 2025"
    width: 1280
    height: 720
    chalkboard: true
    slide-number: c/t 
    theme: Fabio_theme/Fabio_theme.scss
    navigation-mode: linear
    controls: false
    embed-resources: false
    auto-stretch: false
    header-includes:
      - <script src="Fabio_theme/Fabio_theme.js"></script>

editor: source
---

::: {.cell}

:::






##


<div style="position: absolute; top:25%; left:0%; width:"1300px">
  <img src="Additional files/images/diagram_1.png">
</div>

## 


<div style="position: absolute; top:25%; left:0%; width:"1400px">
  <img src="Additional files/images/diagram_2.png">
</div>

## 

<div style="position: absolute; top:15%; left:0%; width:"1400px">
  <img src="Additional files/images/diagram_3.png">
</div>


## Psychometrics

:::: {.columns}
::: {.column width="60%"}

</br>
<center> <div style="font-size: 32px"> The interest of *psychometrics* is uncovering the probabilistic process that <u>**causes**</u> item responses. </div> </center>

</br>

in general, psychometric models are statistical models that predict item responses. All psychometrics models include:

- Parameters defining **item properties**
- Parameters defining **person properties**
- Allow for some **randomness** in item responses (random measurement error)

</br>


<center>
**Item response theory** (IRT) models include all of these components.
</center>

:::
::: {.column width="40%"}

![](Additional files/images/Figure_1.png){width="60%"}

:::
::::

## A Classical IRT Model


According to this IRT model, the *probability* of a correct response to a binary item ($Y = 1$) for person $j$ to item $i$: 

:::: {.columns}
::: {.column width="60%"}

<iframe class="stretch" width="100%" height="470px" src="https://quinix45.github.io/shinylive_apps/2PL_interactive/"> </iframe>

:::

::: {.column width="40%"}

$$P(Y = 1|a_i,b_i,\theta_j) = \mathrm{logit}(a_i(\theta_j - b_i))$$

- $\theta_j$ is an unobserved person trait that explains why responses from the same individuals are similar to each other (i.e., **ability**)

- $a_i$ and $b_i$ are characteristics that vary across items (e.g., items can be more or less difficult)

:::
::::

## Why Psychometrics and IRT?


Not only we explain the observed responses, but we also obtain **item characteristics** and **person characteristics** in the process. 

**Question:** Can we do the same for quantile items in the Forecasting Proficiency Test [FPT\; @Himmelstein_etal_2024]?





## Defining Forecast Accuracy

Responses to FPT quantile forecast items are on very different scale (e.g. dollars/gallon, thousands of dollars, percentages,...). We define the outcome measure, *historically scaled accuracy*, as




$$
Y_i = \frac{\hat{Y}_i - Y_{\mathrm{res},i}}{SD_{Y_{\mathrm{hist},i}}}
$$

:::: {.columns}
::: {.column width="55%"}


<ul style="font-size: 26px">  

<li>  $\hat{Y}_i$: Reported forecast for item $i$ at any quantile.   </li>

<li>  $Y_{\mathrm{res},i}$: The resolution for item $i$.   </li>

<li>  $SD_{Y_{\mathrm{hist},i}}$: The $SD$ of the historical time series of item $i$.    </li>

</ul>


:::
::: {.column width="45%"}

::: {.fragment fragment-index=1}

$Y_i$: SD units away from the resolution.

:::

::: {.r-stack}

::: {.fragment fragment-index=1}



::: {.cell}
::: {.cell-output-display}
![](SPUDM-presentation_files/figure-revealjs/unnamed-chunk-2-1.png){width=960}
:::
:::


:::

::: {.fragment fragment-index=2}



::: {.cell}
::: {.cell-output-display}
![](SPUDM-presentation_files/figure-revealjs/unnamed-chunk-3-1.png){width=960}
:::
:::



:::

::: {.fragment fragment-index=3}



::: {.cell}
::: {.cell-output-display}
![](SPUDM-presentation_files/figure-revealjs/unnamed-chunk-4-1.png){width=960}
:::
:::



:::

:::


:::
::::

## The Proposed Model


We model $Y_{jiq}$, the accuracy of person $j$ to item $i$ at quantile $q$. 


:::: {.columns}
::: {.column width="40%"}

$$Y_{jiq} \sim \mathrm{Student\ T}(\mu_{iq}, \sigma_{ji}, \mathrm{df}_i) \\
\mu_{iq} = b_i + Q_q \times d_i \\
\sigma_{ji} = \frac{\sigma_i}{\mathrm{Exp}[a_i \times \theta_j]}$$


<ul style="font-size: 22px">  

::: {.fragment fragment-index=1}
<li>  $b_i$: item bias (*irreducible uncertainty*) </li>
:::

::: {.fragment fragment-index=2}
<li>  $d_i$: expected quantile distance. $Q_q$ is a vector of constants that ensures monotonicity of $\mu_{iq}$  </li>
:::

::: {.fragment fragment-index=3}
<li>  $\sigma_i$: item difficulty </li>
:::

::: {.fragment fragment-index=4}
<li>  $\theta_j$: Forecasting ability, the only **person parameter** in the model </li>
:::

::: {.fragment fragment-index=5}
<li>  $a_i$: item discrimination (i.e. the magnitude of the effect of $\theta_j$ on $\sigma_i$)  </li>
:::

</ul>


:::
::: {.column width="60%"}

<iframe class="stretch" width="100%" height="550px" src="https://quinix45.github.io/shinylive_apps/t_model/"> </iframe>

:::
::::


## Model Estimation and Item Parameters

All models were estimated in PyMC [@pymc2023] using Markov Chain Monte Carlo (MCMC) estimation (warmup = 1000, draws = 5000, ~ 40 minutes). All Rhats $\leq 1.01$. 

<center>

::: {.fragment fragment-index=1}

![](Additional files/Item_Parameters.png){width=70%}

:::

</center>

## Person Parameter: $\theta$


Distribution of $\theta$ for the 1194 forecasters (better forecasters have higher $\theta$ values).

<center>
![](Additional files/Theta_Figure.png){width=70%}
</center>
<div style="font-size: 16px; margin-top: -16px; text-align:center;"> *note*. The scale $\theta$ parameter was identified by enforcing a standard normal prior.</div>



## Who gets Higher $\theta s$?

Forecasters who consistently approach the expected forecasts are rewarded



:::: {.columns}
::: {.column width="80%"}
<center>
![](Additional files/theta_plot.png){width=75%}
</center>
:::
::: {.column width="20%"}

</br>

<div style="font-size: 22px"> *note*. In the case of the two top panels, missing person forecast were outside the $Y_{jiq} = [-9; 9]$ range.</div>

:::
::::

## Negative Log-Likelihood of $\theta$



Negative log-likelihood function of $\theta$ given item parameters and participant response:

<iframe class="stretch" width="100%" height="500px" src="https://quinix45.github.io/shinylive_apps/MLE_theta_model_t/"> </iframe>


## Predicting Out of Sample Accuracy


<div style="font-size: 24px"> As per the study pre-registration, Waves 1 and 7 responses were treated as *outcome* and Waves 2,4, 6 were treated as *predictors*. </div>


::: {.fragment fragment-index=1}

:::: {.columns}
::: {.column width="30%"}


</br>
</br>

<div style="font-size: 26px; padding-top: 14px;"> **S-scores (SS):** A proper scoring rule that is normally used to score quantile forecasts (smaller SS, better forecast) </div>



:::
::: {.column width="70%"}

![](Additional files/OUS_acc_plot.png){width=95%}

:::
::::

:::

## Stability of Parameters

Given the complexity of the FPT items, item parameters are likely to change depending on many factors. Still, there seems to be reasonable stability even after a month between Wave 1 and Wave 7 (*test-retest*):


::: {.fragment fragment-index=1}
<center>
![](Additional files/W1_W7_parameters.png){width=58%}
<div style="font-size: 14px"> *note*. Only items from Waves 1 and 7. The $a_i$ parameter requires higher sample sizes to stably estimate, so it was fixed to 1. </div>
</center>
:::




## Takeaways

:::: {.columns}
::: {.column width="30%"}

::: {.fragment fragment-index=1}
- The current approach captures meaingful difference across FPT items (i.e., bias, difficulty, discrimination,...)
:::

::: {.fragment fragment-index=2}
- The $\theta$ metric is easily undesrtood and viable for scoring individuals 
:::

:::

:::{.column width="70%"}
:::{.r-stack}

::: {.fragment fragment-index=1 .fade-in-then-out}
![](Additional files/Item_Parameters.png)
:::

::: {.fragment fragment-index=2 .fade-in-then-out}
<center>
![](Additional files/Theta_Figure.png){width=50%}
![](Additional files/OUS_acc_plot.png){width=55%}
</center>
:::

:::
:::

::::


## Acknowledgments

:::: {.columns}
::: {.column width="30%"}



![](Additional files/images/Fordham_University_seal.svg){width=50%}
 
<figure>
  <img src="Additional files/images/Leah.jpg" alt="Trulli" style="width:220px;">
  <figcaption style="text-align:left;">Leah Feuerstahler</figcaption>
</figure>


:::
::: {.column width="70%"}


<center> 
![](Additional files/images/FRI-logo.png){width=50%}

<div style="display:flex; font-size:26px;">
<figure>
  <img src="Additional files/images/Mark.jpg" alt="Trulli" style="width:220px">
  <figcaption>Mark Himmelstein</figcaption>
</figure>
<figure>
  <img src="Additional files/images/Sophie.jpg" alt="Trulli" style="width:220px">
  <figcaption>Sophie Ma Zhu</figcaption>
</figure>
<figure>
  <img src="Additional files/images/Nik.jpg" alt="Trulli" style="width:220px">
  <figcaption>Nikolay Petrov</figcaption>
</figure>
<figure>
  <img src="Additional files/images/Ezra.jpg" alt="Trulli" style="width:220px">
  <figcaption>Ezra Karger</figcaption>
</figure>
</div>
</center>

:::
::::


## References And Contacts

<div id="refs"> </div>



:::: {.columns}
::: {.column width="50%"}


<center>
<div style="font-size: 30px"> Contact: [fsetti@fordham.edu](mailto:fsetti@fordham.edu){target="_blank"}
 </div>
<figure>
  <img src="Additional files/images/Fabio.jpg" alt="Trulli" style="width:280px">
</figure>
</center>

:::
::: {.column width="50%"}

<center>
<div style="font-size: 30px"> **Slides and More** </div>

<figure>
  <img src="Additional files/images/qr_code.svg" alt="Trulli" style="width:320px">

</figure>
</center>


:::
::::
[](){target="_blank"}

