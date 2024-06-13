
todo:
dirbtinio intelekto teorinis pagrindas

Things to read:

* Basal cognition
  Collective intelligence: A unifying concept for integrating biology
  across scales and substrates
  Patrick McMillen & Michael Levin
  https://www.nature.com/articles/s42003-024-06037-4

* 27 papers on LLM from Ilya Sutskever
  https://arc.net/folder/D0472A20-9C20-4D3F-B145-D2865C0A9FEE
  Posted to twitter in April 2024

  This list is presented in conceptual order, moving from the simplest
  ideas and descriptions to the most recent and complex.

  -- The Annotated Transformer
     Undated (2022?) 8 authors from Google Brain
     https://nlp.seas.harvard.edu/annotated-transformer/

     This is big blob of python code, implementing an API to a
     transformer, and includes descriptive text of what each step does.
     All the actual work is done under the covers. If you do not already
     understand how transformers work, you'll learn nothing from this.
     If you want to see what a typical python API might look like,
     this offers one (meagre) possibility.

  -- The First Law of Complexodynamics
     Scott Aaronson -- Shtetl-optimized blog -- Sept 2011
     https://scottaaronson.blog/?p=762

     Entropy is high for random things, and low for simple things.
     The interesting things are in the middle, but we have no formal
     description for this, beyond got intuition. This log article
     provides some intuition, and talks about the problem, withou really
     offering a solution.

     Interesting point here is "complexity" might be "sophistication",
     http://people.cs.uchicago.edu/~fortnow/papers/soph.pdf)
     And that sophistication is possible:
     Gács, Tromp, and Vitányi,
     IEEE TRANSACTIONS ON INFORMATION THEORY, VOL. 47, NO. 6, SEPT 2001 p2443
     "Algorithmic Statistics",
     http://homepages.cwi.nl/~paulv/papers/algorithmicstatistics.pdf

  -- The Unreasonable Effectiveness of Recurrent Neural Networks
     Andrej Karpathy blog  -- May 2015
     https://karpathy.github.io/2015/05/21/rnn-effectiveness/

     This is a programmer-tinkerer's intro to RNN's (LSTM). The claim
     that RNN/LSTM's are extremely good at predicting "what comes next"
     is illustrated with everything from Tolstoy to baby names to source
     code. You won't learn how or why RNN/LSTM's work but you'll be
     impressed by the results.

     The "Further Reading" section gives an excellent overview of the
     state of the art, circa 2015.

  -- Understanding LSTM Networks
     https://colah.github.io/posts/2015-08-Understanding-LSTMs/
     Colah's blog -- August 2015

     RNN's have trouble with long-distance correlations. LSTM's solve
     this problem. The blog entry explains, in a direct, detailed and
     forthright way, how this works. Suitable for all levels of
     sophistication. Well-done, non-intimidating, clear.

     NN's are "generalized perceptrons", intermediating input from
     output with a weight matrix. They're a specific kind of function.

     The R in RNN's statnds for "Recurrent": besides just using a
     single-layer weight-matrix to generate output, there is "feedback"
     connection, which, when loop-unrolled, looks like a feed-forward
     from one instance to the next. This feed-forward provides a way for
     future NN predictions to incorporate info from past inputs. It
     specifies a specific path through which a form of "memory" can
     flow. The isolation of this path greatly (vastly!) reduces the
     the complexity, as compared to an equivalent NN in which all inputs
     and outputs were concatenated into a giant matrix.

     LSTM's trim down the weight matrix more produntly: they propagate
     two signals: the base signal of an RNN, plus a pass-through channel
     that can propagate (or not) older, more distant data. The
     pass-through channel can furthermore be modulated by the current
     input, so that there's both a pass/no-pass gate with an additional
     modulation to inject newer "adjective" modifiers.

     Conclusion on LSTM architectures: they're al about the same for
     quality of results. The GRU, Gated Recurrent Unit, seems to be the
     most refined of the set.

  -- Recurrent Neural Network Regularization
     Wojciech Zaremba, Ilya Suskever, Oriol Vinyals - 2014
     https://arxiv.org/pdf/1409.2329.pdf

     RNN's tend to overfit. Dropout is a good way of avoiding
     overfitting. This explains how to add dropout to RNN's, and
     specifically, LSTM's.

     Provides a nice, easy summary/intro to the more formal math
     notation of LSTM's. Clearer than the Wikipedia LSTM article.

     The dropout operator will *randomly* disrupt cell communications
     between layers (but only the "h" "hidden" path) Ths "c" "cell"
     pass-thru is not disrupted. Time-steps are not disrupted. Only
     inter-layer of "h".

     Quoting: "The dropout operator corrupts the information carried by
     the units, forcing them to perform their intermediate computations
     more robustly."

     Penn Tree Bank: N=2 layers. Loop-unrolled for 35 steps. Dropout rate
     of 50% and 65% (!) Size is 650 units for medium, 1500 for large.

     Macine translation: 4 layers, 1000 units, dropout of 20%

  -- Keeping Neural Networks Simple by Minimizing the Description
     Length of the Weights
     Geoff Honton, Drew van Camp -- 1993
     https://www.cs.toronto.edu/~hinton/absps/colt93.pdf

     Classic paper. Ostensible topic is to avoid overtraining by adding
     Gaussian noise. Actual topic is a strong formal, physical
     (mathematical?) explanation of NN algorithms, using info-theortic
     principles. Training is treated as a compression problem, of
     encoding the training set so as to minimize number of bits to
     describe it. This is a short but hard-to-understand paper.

     Description Length = log(RMS weights) + log(RMS training errors)
     Minimum Description Length (MDL) minimizes the above.

     Instead of thinking of a weight as a single number, think of it as
     a gaussian distribution; training ajusts the mean and width of that
     distribution. In general, the weight distribution is not a single
     mode (not a single gaussian) but a sum of multiple modes (e.g. a few
     weights distributed near 1.0, and many more, distributed near 0.0.)
     In this case, the sum passes over to a Boltzmann distribution.

     ... Oh I can't recapitulate this. It's an opaque paper. It does end
     up arguing that the Helmholtz free energy is what matters, but the
     relationship of this to training errors, weight distributions, and
     minimum desription length remains opaque.

  -- Pointer Networks
     Oriol Vinyals, Meire Fortunato, Navdeep Jaitly (2017)
     https://arxiv.org/pdf/1506.03134.pdf

     First, a sequence-to-sequence map is described. This is as follows:

     Use LSTM to train sequence-to-sequence maps. Input is a sequence of
     N vectors; each vector is assigned an index. The output is a
     sequence of indexes; thus the output has a vocabulary of exactly
     size N; thus the output has a vocabulary of exactly size N.
     (Each distinct size requires distinct training.)

     Two LSTM's are used, an "input" LSTM and an "output" LSTM. The full
     input sequence is run through the input LSTM to produce a single
     vector: the hidden-state vector produced at the end of input.
     This is the encoding of the input; it is handed to the output
     LSTM, to prime it, to produce the output. (which RNN's on "itself")

     During generation, beam search is used.

     The above basic sequence-to-sequence has trouble and gets "blurry"
     for long sequences, because the size of the hidden vector is fixed.
     Thus, an attention model is proposed. This works well, but has
     trouble restricting to the vocabulary.  Finally, the "ptr-net" is
     defined as the attention model w/o one path (see paper for
     formulas.)


  -- ImageNet Classification with Deep CNNs
     https://proceedings.neurips.cc/paper_files/paper/2012/file/c399862d3b9d6b76c8436e924a68c45b-Paper.pdf

     ImageNet, 15 million images, 22K categories, labelled
     ILSVRC is a subset of 1.2 M images, 1K categories of 1K images
     Cropped to 256x256 pixels

     ReLU Rectified Linear Units sigma(x) = max(0,x)
     8 layers, of which 5 Convolutional Net and 3 fully connected.
     1st layer: 224x224x3 input image, 96 kernels, 11x11x3 stride 4 pixels
     So 224x224x3 = 150528 neurons
     Pooling: avg neighborhood of zxz pixels, spaced `s` apart.
     Used s=2, z=3 but s=2, z=2 ok also. Also used normalization.
     2nd layer: 256 kernels of 5x5x48 for 253440 = 256x11x10x9 (?) neurons
     3rd,4th,5th layers, no pooling or normalization
     3rd layer: 384 kernels 3x3x256 for 64896 = 384x12x13 neurons
     4th layer: 384 kernels of 3x3x192 for 64896 = 384x13x13 neurons
     5th layer: 256 kernels of 3x3x192 for 43264 = 256x13x13 neurons
     fully connected layers have 4096 neurons each.
     Total: 60 million parameters
     Overfits, since "only" 1.2M training images

     Combat overfitting by:
      * Translating, reflecting images.
      * By multiplying RGB space with random multiples of
        PCA components of the images.
      * By method of dropouts (killing neurons randomly)

     Gradient descent, weight decay.

  -- Order Matters: Sequence to sequence for sets
     https://arxiv.org/pdf/1511.06391.pdf
  -- GPipe: Efficient Training of Giant Neural Networks using Pipeline Parallelism
     https://arxiv.org/pdf/1811.06965.pdf
  -- Deep Residual Learning for Image Recognition
     https://arxiv.org/pdf/1512.03385.pdf
  -- Multi-Scale Context Aggregation by Dilated Convolutions
     https://arxiv.org/pdf/1511.07122.pdf
  -- Neural Quantum Chemistry
     https://arxiv.org/pdf/1704.01212.pdf
  -- Attention Is All You Need
     https://arxiv.org/pdf/1706.03762.pdf
  -- Neural Machine Translation by Jointly Learning to Align and Translate
     https://arxiv.org/pdf/1409.0473.pdf
  -- Identity Mappings in Deep Residual Networks
     https://arxiv.org/pdf/1603.05027.pdf
  -- A Simple NN Module for Relational Reasoning
     https://arxiv.org/pdf/1706.01427.pdf
  -- Variational Lossy Autoencoder
     https://arxiv.org/pdf/1611.02731.pdf
  -- Relational RNNs
     https://arxiv.org/pdf/1806.01822.pdf
  -- Quantifying the Rise and Fall of Complexity in Closed Systems: The Coffee Automaton
     https://arxiv.org/pdf/1405.6903.pdf
  -- Neural Turing Machines
     https://arxiv.org/pdf/1410.5401.pdf
  -- Deep Speech 2: End-to-End Speech Recognition in English and Mandarin
     https://arxiv.org/pdf/1512.02595.pdf
  -- Scaling Laws for Neural LM
     https://arxiv.org/pdf/2001.08361.pdf
  -- A Tutorial Introduction to the Minimum Description Length Principle
     https://arxiv.org/pdf/math/0406077.pdf
  -- Machine Super Intelligence Dissertation
     https://www.vetta.org/documents/Machine_Super_Intelligence.pdf
  -- PAGE 434 onwards: Komogrov Complexity
     https://www.lirmm.fr/~ashen/kolmbook-eng-scan.pdf
  -- CS231n Convolutional Neural Networks for Visual Recognition
     https://cs231n.github.io/


* Fundamental Components of Deep Learning: A category-theoretic approach
  Bruno Gavranović
  https://arxiv.org/abs/2403.13001

* Neurosymbolic AI: The 3rd Wave
  Artur d'Avila Garcez, Luis C. Lamb
  https://arxiv.org/abs/2012.05876

* Cerebras CS-3 wafer scale engine. 900,000 cores, 44 GB of on-chip
  memory, and 21 petabyes/second of memory bandwidth.
  Announced March 2024
  https://www.cerebras.net/product-system/

* From Conceptual Spaces to Quantum Concepts:
  Formalising and Learning Structured Conceptual Models
  Sean Tull, Razin A. Shaikh, Sara Sabrina Zemljiˇc and Stephen Clark
  https://arxiv.org/abs/2401.08585

* A mathematical perspective on Transformers
  Borjan Geshkovski, Cyril Letrouit, Yury Polyanskiy, Philippe Rigollet
  https://arxiv.org/abs/2312.10794

* General topic keyword hits: "good regulator theorem" and
  "perceptual control theory".

* Gmytrasiewicz, Piotr (August 2020).
  https://www.jair.org/index.php/jair/article/view/11951/26599
  "How to Do Things with Words: A Bayesian Approach".
  Journal of Artificial Intelligence Research. 68: 753–776.
  Modern update to J.L Austin's theory of speech acts/performative
  utterances.

* Bayesian Flow Networks
  Alex Graves et al
  https://arxiv.org/abs/2308.07037

* Distilling Singular Learning Theory. Liam Carroll 2023
  https://www.lesswrong.com/posts/CZHwwDd7t9aYra5HN/dslt-2-why-neural-networks-obey-occam-s-razor

* Alabdulmohsin Ibrahim, 2020, Google,
  "Towards a unified theory of learning and information"
  for the data mining tradition of Vapnik-Chervononkis

* Aschenbrenner, Dolich, Haskell, MacPherson, Starchenko
  "Vapnik Chervonenkis density in some theories without the independence property, I", 2011,
  for the model theoretic tradition.

* Miroslav Benda, "Modeloids. I"
  https://www.ams.org/journals/tran/1979-250-00/S0002-9947-1979-0530044-4/S0002-9947-1979-0530044-4.pdf

* Ologs: A Categorical Framework for Knowledge Representation
  David I. Spivak, Robert E. Kent
  https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0024274

* Evans, R., Bošnjak, M., Buesing, L., Ellis, K., Pfau, D., Kohli, P., & Sergot, M. (2021).
  Making sense of raw input.
  Artificial Intelligence, 299, 103521. doi:10.1016/j.artint.2021.10352

* https://arxiv.org/abs/2006.10637
  Temporal Graph Networks for Deep Learning on Dynamic Graphs

* https://distill.pub/2021/gnn-intro/
  A Gentle Introduction to Graph Neural Networks

* hierarchical message passing: https://arxiv.org/abs/2009.03717

* hypergraphs: https://arxiv.org/abs/1809.02589

* https://github.com/HazyResearch/H3 language model

* A Scalable, Interpretable, Verifiable & Differentiable Logic Gate
  Convolutional Neural Network Architecture From Truth Tables
  Adrien Benamira, Tristan Guérand, Thomas Peyrin, Trevor Yap, Bryan Hooi
  https://arxiv.org/pdf/2208.08609.pdf

* Metagraphs and their applications
  Amit Basu, Rober Blanning, 2007
  Springer Integrated Series on Information Systems.

* https://labs.engineering.asu.edu/labv2/2023-aaai-tutorial-advances-in-neuro-symbolic-reasoning/

* https://github.com/varunshenoy/GraphGPT

* Gato:
  - https://www.youtube.com/watch?v=1lkdWduuN14&t=1577s
  - A Generalist Neural Algorithmic Learner: https://arxiv.org/abs/2209.11142

* Something about category theory:
  - https://www.youtube.com/watch?v=1lkdWduuN14
  - https://cats.for.ai/

* Compositionality
  - https://twitter.com/GaryMarcus/status/1512647983317151747
  - https://compositionalintelligence.github.io/

* Assembly Theory quantifies the complexity of a molecule by finding
  the shortest path to construct the molecule from simple parts. This
  provides the molecular assembly index (MA).

* https://deeplearningtheory.com/
  https://arxiv.org/abs/2106.10165

* In general: backprop, relu, dropout, multi-head attention mixup,
  seperable conv, batch norm layer norm ...

* The Hyperbolic Geometry of Networks
  Prof. Dr. Tobias Friedrich
  https://hpi.de/friedrich/research/hyperbolic-networks/

* COMET : Commonsense Transformers for Automatic Knowledge
  Graph Construction
   https://arxiv.org/pdf/1906.05317.pdf

* "Wide Attention Is The Way Forward For Transformers"
  https://arxiv.org/pdf/2210.00640.pdf

* Nick McKenna Mark Steedman,
  Smoothing Entailment Graphs with Language Models
  https://arxiv.org/pdf/2208.00318.pdf

* https://openai.com/api/ the GPT-3 language model

* Elena Di Lavore, Giovanni de Felice, Mario Román,
  Monoidal Streams for Dataflow Programming
  https://arxiv.org/pdf/2202.02061.pdf
  Appendix contains strong description of monoidal categories.

* https://arxiv.org/pdf/2202.02061.pdf and matching talk
  https://www.youtube.com/watch?v=-pxFgjLJ3uM

* Noam Zeilberger: "Parsing as a lifting problem and the
  Chomsky-Schützenberger representation theorem"
  Topos Institute
  https://youtu.be/AX8tpQSi8v8

  Colloquim.

* Michel TALAGRAND, Mean Field Models for Spin Glasses Volume I:
  Basic Examples (2010) Book, 490 pages, Springer-Verlag

  Provides overview of high-dimensional statstics from a mathematicians
  viewpoint. Sherrington-Kirkpatrick model, Perceptron, Hopfield,
  and more.

* Kyle Marple, Elmer Salazar, Gopal Gupta,
  Computing Stable Models of Normal Logic Programs Without Grounding
  arXiv:1709.00501v1 [cs.LO] 1 Sep 2017
  https://arxiv.org/pdf/1709.00501.pdf

  How to get ASP solving without ASP.

* ASP inside of SWI-prolog https://github.com/JanWielemaker/sCASP

* Gao et al,
  The Pile: An 800GB Dataset of Diverse Text for Language Modeling
  https://arxiv.org/abs/2101.00027

* Leo Gao,
  An Empirical Exploration in Quality Filtering of Text Data
  https://arxiv.org/abs/2109.00698
  We find that aggressive filtering can in fact lead to a decrease
  in model quality on a wide array of downstream tasks for a
  GPT-like language model.

* Paul Cisek - papers on neural evolution.

* Tobias Fritz
  A synthetic approach to Markov kernels, conditional independence
  and theorems on sufficient statistics.
  https://arxiv.org/abs/1908.07021v8

* Tailin Wu, Max Tegmark
  Toward an AI Physicist for Unsupervised Learning
  https://arxiv.org/abs/1810.10525
  Unsupervised learning of theories and their domain of validity.

* Multimodal Neurons in Artificial Neural Networks
  https://openai.com/blog/multimodal-neurons/
  This is an important foot-in-the-door for symbolic reasoning.

* Geoffrey Hinton
  "How to represent part-whole hierarchies in a neural network"
  25 Feb 2021
  https://arxiv.org/abs/2102.12627

* Robert Haas
  Symbolic regression with Atomese code in OpenCog
  https://robert-haas.github.io/g3p/media/notebooks/atomese_symbolic_regression.html
  Python notebook, doing moses-like symbolic regression.

* Henry W. Lin, Max Tegmark, and David Rolnick
  "Why does deep and cheap learning work so well?"
  3 Aug 2017
  https://arxiv.org/pdf/1608.08225.pdf

  Short answer: they can represent multiplication and addition easily.
  Making them deep makes the representation more compact and simpler to
  learn.

* Harmen Prins
  "Matching ontologies with distributed word embeddings"
  July 7, 2016
  http://www.ru.nl/publish/pages/769526/z_harmen_prins.pdf

* David I. Spivak, Nelson Niu
  "Polynomial Functors: A General Theory of Interaction"
  2021
  https://topos.site/poly-book.pdf

  Knee-jerk reaction after a 30-second skim: yes, exactly! Polynomials in
  N variables lead naturally to the idea of jets and sheaves. So the sheaf
  theory that is being developed in this project is what you would get if
  you took a polynomial, and threw away the addition and multiplication,
  and replaced them by concatenation and composition.

* Grant Sanderson
  "Puzzling through exact sequences"
  11 Nov 2021 3Blue1Brown blog
  https://www.3blue1brown.com/blog/exact-sequence-picturebook

  Jigsaw puzzle pieces! This time, they occur in algebraic topology!

* AMR "Parsing with Action-Pointer Transformer" - 24 Nov 2020
  https://openreview.net/forum?id=X9KK-SCmKWn

* Mark Newman, Albert-László Barabási, and Duncan J Watts.
  "The structure and dynamics of networks."
  Princeton University Press, 2006.

* John Baez
  "Toplogical Crystals"
  2016
  This is interesting because it discusses the treatment of covering
  spaces with vectors. This is similar to what is being done in this
  project.  However, in this project, its not clear that deck
  transformations are meaningful or carry something important, although
  perhaps the exchange of synonymous phrases can be treated as a deck
  transformation.

* Russ Harmer and Eugenia Oshurko "Reversibility and composition of
  rewriting in hierarchies", (2020)
  https://hal.archives-ouvertes.fr/hal-02869865

  The idea here is that sequences of rules can be applied, and then they
  can be reversed. This enables back-tracking on a rule system.

* D. A Turner, "Functional programs as executable specifications"
  (1984) Phil. Trans R. Soc. Lond. A 312, pp. 363-388.

  Functional programming languages as a collection of rewrite rules
  applied to program expressions. Background theory for the Clean
  programming langauge. See
  https://en.wikipedia.org/wiki/Clean_(programming_language)
  See also the Wikipedia description of the ABC machine.

  The relevance for the AtomSpace is this: Deep inside a query, we
  need to be able to call out to functions, passing arguments. For
  opaque, black-box functions, we need to eager-evaluate all arguments.
  There are two alternatives to eager evaluation. One is to do a $vau
  trick, and eager-evaluate only the first arg, and pass the rest
  unevaluated. The other is to evaluate none of them, and pass a context
  which contains all necessary symbol groundings are present, so that
  the callee can evaluate, as needed.

  I dunno. Feels like we're reinventing... Hmm.

* HodgeNet -- Justin Solomon -- learning of the Hodge star operator
  via neural nets.  Specifically, for dealing with sparse matrices.

* Max Tegmark
  Consciousness as a state of matter
  https://www.sciencedirect.com/science/article/abs/pii/S0960077915000958

Actually useful in practical applications
-----------------------------------------
* Jesse O. Wrenn, Peter D. Stetson, Stephen B. Johnson
  An Unsupervised Machine Learning Approach to Segmentation of Clinician-Entered Free Text
  AMIA Annu Symp Proc. 2007; 2007: 811–815.
  PMC2655800
  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2655800/

  Lists the following prior work:
  * Zellig Harris (1967) -- Compute conditional probability of transitions
    from character to character. Minimize entropy to obtain morpeheme boundaries.

  * John Goldsmith (2001) -- Minimum Description Length

  * Mollah & Johnson (2003) -- Use Harris algo to get morphemes, then prune
    based on MI.

  Current work:
  * Compute the conditional probablity of observing a character, given a
    preceeding string.  Cap at length seven. Go forwards and backwards.
  * "Freedom at character transitions" == number of distinct characters that
    can follow a given string. There's a forward and backward version of this.
  * "Peak freedom" is a second difference; its the sum of the increase in
    freedom of the prior transition, and the decrease in freedom of the
    subsequent transtion.   There are both forward and backward peak freedoms.
  * Two paramters control tokenization: substring length (its a priori) and
    cutoff for peak freedom.
  * Characterization of results is conventional b.s. mumbo-jumbo tables
    showing sensitivity, specificity, and area under ROC curve. This is
    infuriating, because this kind of conventional analysis completely
    obscures what is actually happening! Argh!

  Suggested clarifications to above work:
  * A precise mathematical, symbolic definition of peak freedom is needed.
    The informal definition is imprecise, prone to misunderstanding, and
    obscures relationships to other similar mathematical formulas.
  * Peak freedom appears to be defined as a second difference, as the
    difference between the prior and the subsequent transition freedom.
    What if, instead, one worked with log2 of transition freedoms, so
    that a peak entropic freedom was a ratio instead of a difference?
  * Given this variant, what other variants of freedom are possible, and
    how are they related to more traditional entropic definitions?
  * Show distribution of peak freedom. That is, there are hundreds of
    thousands of transitions; what's the freedom of each? What's the peak
    freedom of each?
  * How do things change, if one considers not just the next character, but
    the next pair of characters, or the next triple of characters?

The Big Picture
---------------
Plant morphogenesis
 * P. Prusinkiewicz
   http://algorithmicbotany.org/papers/

Animal morphogenesis
 * Darwin’s agential materials: evolutionary implications of multiscale
  competency in developmental biology
  Michael Levin
  https://link.springer.com/content/pdf/10.1007/s00018-023-04790-z

Evolution of Brains
 * "Forced moves or good tricks in design space?  Landmarks in the
    evolution of neural mechanisms for action selection", Tony J. Prescott
    (2007) https://www.academia.edu/30717257/Forced_Moves_or_Good_Tricks_in_Design_Space_Landmarks_in_the_Evolution_of_Neural_Mechanisms_for_Action_Selection


General Background
------------------
Foundational texts.

* Peter Norvig, Stuart Jonathan Russell,
  Artificial Intelligence: A Modern Approach
  https://libgen.li/edition.php?id=138615766

  Basic textbook. Everyone should read it, beginning to end.

  PDF and TOC page numbers do not align. Some cross-referencing:
  Chap 13: TOC page 412 vs PDF page  780 - Probabilistic reasoning
  Chap 20: TOC page 721 vs PDF page 1329 - Learning Probabilistic Models
      - Expectation Maximization algo: 1362, 1369
  Chap 21: TOC page 750 vs PDF page 1378 - Deep Learning
  Chap 22: TOC page 789 vs PDF page 1449 - Reinforcement Learning
  Chap 23: TOC page 823 vs PDF page 1512 - Natural Language Processing
  Chap 24: TOC page 856 vs PDF page 1573 - Deep Learning for Natural Language Processing
  Chap 25: TOC page 881 vs PDF page 1616 - Computer Vision
  Chap 26: TOC page 925 vs PDF page 1689 - Robotics
  Chap 27: TOC page 981 vs PDF page 1791 - Conclusions


Useful for making specific claims
----------------------------------
* Claim: there is no such thing as "context" in natural language;
  instead, it is constructed by listener and speaker, with both
  having "considerable disgression" in so doing.
  Graeme Hirst, "Context as a Spurious Concept", (1998)
  https://www.academia.edu/49955919/Context_as_a_Spurious_Concept


Suggestive but Important
------------------------
Don't quite provide what is immediately needed, but is very interesting
anyways:

* Pentti Kanerva, "Hyperdimensional Computing: An Introduction to Computing
  in Distributed Representation with High-Dimensional Random Vectors"
  Cognitvie Compututing (2009) 1:139–159 DOI 10.1007/s12559-009-9009-8

  Provides a good, general description of hypervector memory and
  hypervector ALU operations. Informal (few formulas) but mostly
  accurate (I spotted a few mistakes).

  Basic ideas: high-dimensional binary vectors (hypervectors), general
  stochastic properties of hyperspace, content-addressable memory, ALU
  ops as XOR and vector addition. Collision-avoidance via permutations.

  Examples: encoding sets as hypervectors, encoding pairs, encoding
  tables, encoding sequences. (and reconstructing these, given the
  hypervectors)

  Practical examples: hypervectors as alternative to LSA (Latent
  Semantic Anslysis) overcomes difficulties of PCA (Principle Component
  Analysis). Also, document-vectors look like context vectors, i.e.
  the two approaches become unified with hypervectors.

  Author recognizes the role of grammar, without saying the word
  "grammar" and suggests word contexts are impoverished. (and this is
  correct; so he punts.)

  Practical example: encoding of logical inference. Appears to be a
  variant of encoding a table. Points out that naive representations
  struggle with certain kinds of relationships (transitivity,
  membership.) Implies more complicated representations can over come this?
  (without showing how).

  Practical example: learning by example.  Not this is actually
  interesting.  Shows how basic relationships can be infered from
  a handful(?) of examples.  Given the difficulty of inferencing
  described above, this is a crude approximation to a more fully
  formal, symbolic relational description. However, I'm thinking
  that this is enough to get started, to provide the needed rough
  draft that other kinds of systems can take over and elaborate.
  Wow! That's actually pretty cool! I'm psyched!

  Concludes with a nice summary of historical developments of
  various vector-encoding schemes.


Interesting, but maybe less useful
----------------------------------
* Logical Decision Theory
  An Introduction to Logical Decision Theory for Everyone Else
  https://arbital.com/p/logical_dt/?l=5kv
  Attempts to provide a foundation for why voting in elections is important.
  As best as I can tell, the argument is this:
   -- It is impossible to know the entire light-cone of history of an agent.
   -- Yet such a history would be required to estimate a probability
      P(x|history) of making decision x.
   -- Approximate `history` by a model of an agent of a specific type.
   -- Assume all agents of a specific type react in the same way to
      some new, novel situation.
   -- Any agent with imperfect knowledge should create such estimates or
      'models' of other agents, and base decisions on those models.
  The actual argument they make is far narrower: they implement a
  PrudentAgent that cna play prisoner's dilemma, and uses first-order
  logic to understand (make models of) other players. This PrudentAgent
  is a demo tool that can be used in economics theory to solve some
  previously intractable economic theory regarding cooperation.

* Jerry R. Hobbs "Chapter 6: Word meaning and world knowledge"
  (2019) in book "Semantics - Theories", de Gruyter
  https://doi.org/10.1515/9783110589245-006

  A fairly general overview of the relationship between the meaning
  of words, and world models. Taken as a general overview of the
  general problem, the text seems adequate.

  I'm worried that it is suggesting perhaps exactly the wrong direction?
  More precisely, the goal of the unsupervised learning project to
  "learn common sense", and to "learn logical thinking", and so it
  is fundamentally wrong to start with a model of semantics that is
  trying to jam meaning into a pre-existing logical framework.  Notions
  such as scalars, magnitudes, causality, changes of state are to be
  *learned* and *not* programmed in. This includes ontological
  structure: is-a, has-a relationships: these are to be *learned*,
  and not hard-coded into the theory.

* Mati Kilp, Ulrich Knauer, Alexander V. Mikhalev.
  "Monoids, acts, and categories : with applications to wreath products
  and graphs : a handbook for students and researchers"
  (2000) W. de Gruyter,

  Provides a handbook on the basic defintions of state machines, and
  how one talks about them in a category-theoretic language.  Useful,
  in that it provides a general background in category theory to core
  concepts regarding monoids.

* Alberto Speranzon, David I. Spivak, Srivatsan Varadarajan
  "Abstraction, Composition and Contracts: A Sheaf Theoretic Approach"
  8 Feb 2018
  https://arxiv.org/abs/1802.03080

  The keyword in the title is "contracts". Although the paper uses
  sheaf theory as the abstact foundation for composition, the primary
  application of the abstract is to glue together computational models
  of complex systems-of-systems, with particular attention paid to
  discrete and continuous (dynamical) systems. This is all very nice, but
  does not appear to be relevant to the present concerns: that of
  discerning meaningful structure from probabilistic observations.

  The primary astraction presented in the paper is the sheaf of continuous
  intervals on the real number line. These are then used to glue together
  continuous-time dynamical systems and discrete-time decision systems;
  the primary example is an aircraft collision-avoidance system, where the
  continous parameters include height, speed and heading, and the discrete
  decisions to be made include banking left or right, or changing
  altitidue.

  There is no obvious way to convert that discussion into one that glues
  together probabilistic relationships to graphical representations. The
  keyword here is "obvious": clearly, information-theoretic quantities are
  continuous, and symbolic reprsentations are discrete. How can
  sheaf-theoretic framing provide insight? Not obvious. Would take
  significant effort.

  One of the more interesting claims (theorems) is that the sheaf of
  integer-length intervals is equivalent to the category of graphs.
  The sheaf axioms instruct how intervals can be glued to one-another in a
  coherent fashion.  The intervals, so glued, correspond to paths on a
  graph.  This is an interesting equivalence, and symbolic representations
  of meaning certainly seem to be kinds of graphs. However, nothing in the
  symbolic representations seem to correspond to paths on graphs, the
  linear nature of sentences notwithstanding.  The grammatical structure
  of a sentence is not overtly a walk of a graph; it is a flattening, a
  serialzation of a more complex structure.  It is not obvious that the
  equivalence of the sheaf of intervals and the caegory of graphs offers
  any articular insight into either grammatical sructure, or into the more
  general setting of Curry-Howard correspondance.

* Scott Garrabrant, Tsvi Benson-Tilsen, Andrew Critch, Nate Soares, and Jessica Taylor
  "Logical Induction" (2016)
  https://arxiv.org/abs/1609.03543
  "Logical Induction (Abridged)" (2016)
  https://intelligence.org/files/LogicalInductionAbridged.pdf

  "We present a computable algorithm that assigns probabilities to
  every logical statement in a given formal language, and refines
  those probabilities over time."

  Interesting, but some of the claims appear to be false.  Page 12 states
  (seems to state, when I read it) that theorems that can be efficiently
  enumerated will be assigned a high price in relatively short order.
  But surely this cannot be the case, or I misunderstand? If I can
  efficiently enumerate a sequence of sentences `S`, then for any given
  sentence `s` I can propose that `s is true` and `s is false` at the same
  time. But both of these cannot be simultaneously assigned a high price.
  Do I misunderstand something? (Probably.)

  The example of Ramanujan and Hardy clarifies the intent: We now
  understand that Ramanujan could do long division in his head, and thus
  numerically calculate sequences of numbers, and thereby find patterns.
  As a pattern recognizer, he could generate "true" theorems faster than
  Hardy could prove them.  That is, Ramanujan is not just enumerating
  theorems (as that would require enumerating both the theorem and its
  negation), but is instead spotting patterns.

  At any rate, betting on mathematical theorems seems like a dubious
  activity. When is it actually useful to do so?  For example, number
  theory is filled with black swan events: things that appear to follow
  a predictable pattern, until they do not. I can see why rational
  traders would assign these a high price until such time when a
  counter-example is found, but it seems a bit dubious to try to assign
  a price to such statements, unless it was a "bet your life" situation.

  Thus, the algorithm they propose is interesting not so much because it
  provides a betting pool for mathematical theorems, than it is a means of
  discerning structure in (formal) languages.  Kind of like what this
  project (`opencog/learn`) is trying to do.

* GPT-4 System Card, OpenAI March 15, 2023

  Most interesting thing about this is the dozen-plus extensive examples
  of prompt programming/prompt-engineering/prompt-coding. The rest of
  the paper is bizarrely off-kilter. They seem to be remarkably cluelesss
  about what AI safety is suppose to be, even though that is ostenisibly
  the topic. Unimaginative, dazed and confused, dettached from reality.
  What the heck. Beats me. So, two basic remarks, neither of which is
  covered in the paper; these are my interpretation only.

  Remark 1) Prompt programming. This appears to be a collection of
  way-points in the GPT hypercube, a directed path, from here to there,
  with a handful of side branches/tracks. That path, and everything
  geometrically close to it, is meant to block out some objectionable
  patch of word-sequences. In short: treat GPT as a metric space;
  given any word-sequence (e.g. a question), the GPT weights specify all
  other nearby word sequences For example, "who threw the ball?" and
  "John threw the ball" have a hamming distance of 1, and thus the
  question can be trivially answers by just looking for other nearby
  sequences. Of course, GPT scales this up to Hamming distances of 100's
  or more not 1; the distances include both grammatical similarity,
  synonymous phrases, contextual embeddings (last few dozen/hundred
  words) etc. But, for all intents and purposes, it seems to be an
  extremely high dimensional metric space with the weights providing an
  invariant measure (the "ground state", aka the Haar measure.) The
  prompt code specifies a region of that space, a tube, of the same
  shape as the prompt. Everything in that tube is marked as
  objectionable content, and is blocked with a generic "I'm sorry,
  I can't talk about illegal/immoral/hurtful acitivty X". The prompts
  are vividly detailed; there a dozen or so. There's absolutely no
  effort made to demonstrate the corresponding tube, the haar measure,
  or to measure the width or length of the black-out tube. To
  characterize its volume, metric properties, curvature, attractivity,
  nothing whatsoever. Don't they have scientists working there? Why
  didn't they report any of this? Is it confidential? Big spinning WTF???

  Remark 2) Interacting with GPT-3 is a singularly toxic silicon valley
  tech bro experience. If GPT was a person and they came to a party I
  was hosting, I would find a baseball bat, and threaten it until it
  left. Not only is it as dumb as a rock, but it looks you in the eye
  as it lies to your face. WTF. I do appreciate that GPT and DL/NN in
  general is an important fundamental breakthrough in AI, but wow, it
  has the personality traits of a fucktard shithead. And so suddenly
  I'm thinking about a Julia Mossbridge/Ben Goertzel production called
  Loving AI http://lovingai.org/ It takes the Hanson Robotics Sophia
  Robot and turns it into a meditation guru. Now, this is entirely
  scripted; there's very little "hard AI" in it; its designed to produce
  a relaxed, meditative, mildly hypnotic state. Great for mindfulness
  retreats and gets you the honor of meditating with a robot. Pretty
  distant from my wheelhouse, but it was .. intesting, anyways. Well,
  I'll tell you. After that toxic encounter with chatGPT, the system
  that knows everything and the value of nothing, ... it occurs to
  me that loving AI is absolutely the correct direction to move in.
  GPT is fascinating as a technology, and fundamentally evil in it's
  incarnation. Now, I have no clue at all how to actually, technically
  make loving AI be a true, "hard" AI system that actually "thinks"
  (whatever the heck that means) and is "nurturing" (unclear what
  that is), but as of today, this is what I will be pondering.

* Parsing using a grammar of word association vectors
  Rob Freeman
  https://arxiv.org/abs/1403.2152

  Word asssociation vectors. A contemporaneous accoount of ideas
  broadly similar to those in this project; although sharply different
  in actual details.

----------
