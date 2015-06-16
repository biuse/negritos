       NEURAL NETWORK, main program

             DATA: Data files must have the same root name with _train.txt and _test.txt. The file format will be outputs, inputs. If a classification network is needed then first column will be the class where the inputs belong. (from 0 to Nclass)

             NETWORK: with any number of hidden layers each with a given number of hidden nodes. Activation function would be tanh for all of them. If this needs to be change, we need to rewrite subroutine nnet_layer and grad_layer. Network outputs are defined as:
             y_i=sum(w_ij*tanh(sum(w_jk*x_k+theta_j)+theta_i
      
             and the error is computed and minimized
         
             E=0.5*sum(y_i-t_i)**2  (REGRESSION NETWORK)
             
             E=-sum(t_i*ln(y_i))    (CLASSIFICATION NETWORK)

             where t_i and x_k are the inputs and outputs known used in the training. 

            finally E=E+sum(0.5*lambda*weights²)

            The lamda*w_a**2 is a term added to avoid large number of weights. Depends on the problem and the optimizer method. Lambda is set to 0, but if weights get too big, increase it.
            

             INPUTS:
             name_in = root of name of training and testing data and root for other outputs. train data must be a file named 'name_in'//_train.txt, test data 'name_in'//_test.txt'

             This are general variables that are common in all modules.

             User%nlayer = number of layers (without the output layer)
             user%nhid_vec = here we give nin, nhid1, nhid2,...nhidnlayer,nout. Ex. if layer is 3:  20 2 1 3
             User%opt_net = 0 for a classification network 1 for a regression netowrk
             User%opt_alg= Optimiation algorithm:
             0. Gradient conjugate (NR) 
             1. Levenberg-Marquardt method (MINPACK)
             2. Powell method (NR)
             3. BFGS method (dfpmin subroutine NR)
        
             (Fastest for regression problems leveberg-Marquardt. Best results and fastest for classification and regression high-dimension problems BFGS method)

              User%opt_wgt = 0 to start training from scratch (weights will be initialized by the Nguyen-Wadrow inizialization technique). 1 if previous computed weights are used as initial values.
              
              maxeval = number of iterations at which we want to see the current performance of the network.


              OUTPUTS:
              
              scale_files: The program will give a 'name_in'\\_scales_in.txt an 'name_in'\\scales_out.txt outputs files with the mean value and sigma of training data. This files are needed when we use the prediction code. This is because the inputs are normalized in order to avoid linear dependence between inputs.

              weights_files: A weight file at each epoch will be generated. This is the main result of a network. Then those weights are used for the prediction code.

              performance_file: Correlation coefficient or true positive rate at each epoch is saved. The same that are printe on screen. In case of overfitting is very useful to have a control.

              output_file: An output file with the outputs given for the testing set once the network is trained.

              Correlation : printed on screen. The Pearson correlation coefficient is printed to see the network perfomance (just for User$opt_net=1)
 
              True positive rate : printed on screen if User%opt_net=0. Shows the number of right classified objects by the network.


  CODE VARIABLES SUITABLE FOR CHANGE:
  ftol = is the tolerance of the error we want. Now is set to 1e-4 that is usually enough. 
  lambda = penalizes high weight values. Set to 0
  alpha = value needed if working with entropy where now the function that will be minimized is Q=-alpha*S+E 
  stopping = network will stop at a given number of non-imporved results.

