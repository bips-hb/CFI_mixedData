import numpy as np
import pandas as pd
import torch
torch.cuda.empty_cache()
#from torch import multiprocessing
from DeepKnockoffs  import KnockoffMachine
from DeepKnockoffs import GaussianKnockoffs
from multiprocessing import set_start_method
import multiprocessing
import os

#just for debugging
#instance_np =  np.random.choice(4, size = (1000,5))
#instance = pd.DataFrame(instance_np).rename(columns=lambda x: "x" + str(x))
# instance.columns
# instance = pd.get_dummies(instance.astype(str), drop_first=True) # drop first true = dummy, false = one hot

print('TestPrint before function call')  
def generate_deep_knockoffs(instance):
    torch.set_num_threads(1)
  #  print(torch.get_num_threads())
  #  set_start_method("spawn", force=True )
  #  multiprocessing.get_context("spawn")
    
  #  os.environ['OMP_NUM_THREADS'] = '1'
  #  os.environ['OPENBLAS_NUM_THREADS'] = '1'
  #  os.environ['MKL_NUM_THREADS'] = '1'
  #  os.environ['VECLIB_MAXIMUM_THREADS'] = '1'
  #  os.environ['NUMEXPR_NUM_THREADS'] = '1'
  #  os.environ['CUDA_LAUNCH_BLOCKING'] = '1'
    save_colnames =instance.columns
    instance = np.array(instance)
    SigmaHat = np.cov(instance, rowvar=False)
    ## Gaussian Knockoffs model
    second_order = GaussianKnockoffs(SigmaHat, mu=np.mean(instance,int(0)), method="sdp") # just use this program to get penalty for pairwise corr
    # knockoffs are not uniquely defined -> get knockoffs that penalize the pairwise corr between variables and their knockoffs -> target corr
  #  Xk_gaussian = second_order.generate(instance)
  #  Xk_gaussian = pd.DataFrame(Xk_gaussian)
    # Measure pairwise second-order knockoff correlations
    corr_g = (np.diag(SigmaHat) - np.diag(second_order.Ds)) / np.diag(SigmaHat)

    print('Average absolute pairwise correlation: %.3f.' %(np.mean(np.abs(corr_g))))
 #   return(np.mean(np.abs(corr_g)))

   ## Machine Knockoff model

  # model = "gaussian"
  # distribution_params = {"model": "gaussian", "p": instance.shape[1], "rho": 0.5}
   # Load the default hyperparameters for this model
    training_params = {'LAMBDA':1.0,'DELTA':1.0, 'GAMMA':1.0 }
   # Set the parameters for training deep knockoffs
    pars = dict()
   # Number of epochs
    pars['epochs'] = 10
   # Number of iterations over the full data per epoch
    pars['epoch_length'] = 50
   # Data type, either "continuous" or "binary"
    pars['family'] = "continuous"
   # Dimensions of the data
    pars['p'] = instance.shape[1]
  # Size of the test set
    pars['test_size']  = int(0.1*instance.shape[0])
   # Batch size
    pars['batch_size'] = 64 # works but super slow
    #pars['batch_size'] = int(0.45*instance.shape[0])
   # Learning rate
    pars['lr'] = 0.01
   # When to decrease learning rate (unused when equal to number of epochs)
    pars['lr_milestones'] = [pars['epochs']]
   # Width of the network (number of layers is fixed to 6)
    pars['dim_h'] = int(10*instance.shape[1])
   # Penalty for the MMD distance
    pars['GAMMA'] = training_params['GAMMA']
   # Penalty encouraging second-order knockoffs
    pars['LAMBDA'] = training_params['LAMBDA']
   # Decorrelation penalty hyperparameter
    pars['DELTA'] = training_params['DELTA']
   # Target pairwise correlations between variables and knockoffs
    pars['target_corr'] = corr_g
   # Kernel widths for the MMD measure (uniform weights)
    pars['alphas'] = [1.,2.,4.,8.,16.,32.,64.,128.]

   # Initialize the machine
    machine = KnockoffMachine(pars)

   # Train the machine
    print("Fitting the knockoff machine...")

    machine.train(instance) ########## error that the code gets stuck arises here!!!
    #return("test return")
 # 
 #    # Generate deep knockoffs
    Xk_machine = machine.generate(instance)
    Xk_machine = pd.DataFrame(Xk_machine)
    Xk_machine.columns = save_colnames
  #  print("Size of the deep knockoff dataset: %d x %d." %(Xk_machine.shape))
    return(Xk_machine)
        

#generate_deep_knockoffs(instance)
#generate_deep_knockoffs(r.ex2['test'])

