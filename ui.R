# shiny app to draw distributions

#
# initial entry data
#
# Size of simulated data
n.sim <- 1000
#n.obs <- 20 # unchanged during run
# initial parameters for distributions
normal.mean <- 0
normal.sd <- 1
exponential.rate <- 1
binomial.size <- 20
binomial.prob <- 0.5

shinyUI(pageWithSidebar(
  
  headerPanel(title=h2('CLT demonstration', align='center'), windowTitle='CLT'),
  
  sidebarPanel(
    
    radioButtons(inputId='dist', label='Distribution', 
                 choices=c('Normal', 'Exponential', 'Binomial'), selected='Normal'),
    
    radioButtons(inputId='sample', label='Size of sample', 
                 choices=c('100', '1000', '10000'), selected=1000, inline=TRUE),
    
    uiOutput(outputId='mean'),
    uiOutput(outputId='sd'),
    
    conditionalPanel(condition="input.dist=='Normal'",
                     sliderInput(inputId='mean', label='Mean', value=normal.mean, min=-100, max=100, step=0.01),
                     sliderInput(inputId='sd', label='Standard Deviation (>0)', value=normal.sd, min=0.01, max=100, step=0.01)
    ),

    conditionalPanel(condition="input.dist=='Exponential'",
                     sliderInput(inputId='rate', label='Rate (>0)', value=exponential.rate, min=0.01, max=10, step=0.01)
    ),

    conditionalPanel(condition="input.dist=='Binomial'",
                     sliderInput(inputId='size', label='Size (integer, >0)', value=binomial.size, min=1, max=100, step=1),
                     sliderInput(inputId='prob', label='Probability (>=0, <=1)', value=binomial.prob, min=0, max=1, step=0.01)
    ),
    
    #checkboxInput(inputId='default', label='Reset to defaults', value=FALSE),

    br()
    
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel(
        title='Instructions',
        withMathJax(),
        h4('The Central Limit Theorem'),
        p('The Central Limit Theorem (', strong('CLT'), ') postulates that the mean ',
          'of samples of independent and identically distributed (', strong('IID'), ') random variables ',
          'of known distribution are approximately distributed as normal IID variables ',
          'with mean equal to the mean of the original distribution and variance ',
          'equal to the variance of the distribution divided by the size of each sample:',
          '$$x \\sim N(\\mu, \\frac{\\sigma ^ 2}{n})$$',
          'where \\(\\mu\\) is the mean of the distribution, \\(\\sigma ^ 2\\) its variance and \\(n\\) the size of the sample.'),
        h4('The exercise'),
        p('This Shiny application illustrates the CLT for three different distributions and ',
          'three different number of draws. The distributions are the normal one, the ',
          'exponential one and the binomial one; the number of draws are 100, 1,000 and 10,000.',
          'In all cases, for simplicity, the size of each simulated sample is 20. ',
          'The higher the number of draws, the better the means of the draws approximate a normal ',
          'distribution.'),
        p('The chosen distribution and sample size is simulated for as many times as the ',
          'chosen number of draws, and the mean is calculated for each draw, giving as many ',
          'means as number of draws. This set of means should be distributed approximately as ',
          'a normal.'),
        p('The parameters to input depend on the underlying distribution.'),
        p('For the normal distribution, the mean \\(\\mu\\) and the standard error \\(\\sigma\\)',
          'are needed.'),
        p('For the exponential distribution, a parameter \\(\\lambda\\) is needed,',
          ' indicating the rate of decay in the probability of an event occuring as time goes by.',
          ' The corresponding mean and standard error are both \\(\\frac{1}{\\lambda}\\).'),
        p('For the binomial distribution, two parameters are needed: the number of instances',
          ' the random variable has been observed (the size), \\(n\\), and the probability of a given ',
          ' event happening, \\(p\\). The corresponding mean and standard error are \\(n p\\) and',
          ' \\(\\sqrt{n p (1 - p)}\\), respectively.'),
        helpText('NB: The standard error of the simulated means are the given standard errors',
                'divided by \\(\\sqrt{n}\\).')
      ),
      
      tabPanel(
        title='Distribution of means (CLT)',
        plotOutput("means"),
        verbatimTextOutput(outputId="text0"),
        verbatimTextOutput(outputId="text1"),
        helpText('The asymptotic normal \\(N(\\mu, \\frac{\\sigma}{\\sqrt{n}})\\) ',
                 ' distribution is given by the red line.'),
        br()
      )
    ),
    
    br()
    
  )
  
))
