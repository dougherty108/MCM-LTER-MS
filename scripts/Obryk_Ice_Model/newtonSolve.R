newtonSolve <- function(F, x, options = list()) {
  # Default options
  defaultopt <- list(
    atol = sqrt(.Machine$double.eps),
    etamax = 0.9,
    rtol = sqrt(.Machine$double.eps),
    lmaxit = 40,
    lmeth = 1,
    maxit = 40,
    restart_limit = 20,
    showiters = 0
  )
  
  options <- modifyList(defaultopt, options)
  
  atol <- options$atol
  etamax <- options$etamax
  lmaxit <- options$lmaxit
  lmeth <- options$lmeth
  maxit <- options$maxit
  rtol <- options$rtol
  restart_limit <- options$restart_limit
  debug <- options$showiters
  
  if (debug == 1) {
    cat("Newton-Krylov solver\n")
    cat(" Iteration\tResidual\tInner It.1\tRelative res.\tInner It.2\n")
  }
  
  it_histx <- matrix(0, nrow = maxit, ncol = 3)
  x_hist <- NULL
  
  # Parameters for iterative methods
  gmparams <- c(abs(etamax), lmaxit, restart_limit, 3)
  
  res <- 1
  iter <- 0
  
  # Evaluate F at the initial iterate
  f0 <- F(x)
  old_res <- res
  res <- norm(f0)
  
  it_histx[iter + 1, 1] <- res
  it_histx[iter + 1, 2] <- 0
  it_histx[iter + 1, 3] <- 0
  
  stop_tol <- atol + rtol * res
  
  while (res > stop_tol && iter < maxit) {
    residualRatio <- res / old_res
    old_res <- res
    iter <- iter + 1
    
    # Compute the Newton step
    dx_info <- tryCatch({
      krylovSolve(f0, F, x, gmparams, lmeth)
    }, error = function(e) {
      stop("Newton solver failed in the Krylov step")
    })
    
    dx <- dx_info[[1]]
    errstep <- dx_info[[2]]
    inner_it_count <- dx_info[[3]]
    
    if (debug == 1) {
      if (iter == 1) {
        cat(sprintf("%8i\t%5.1E\t%5i\t%15.1E\t%9i (Input point)\n", 0, res, 0, 1, 0))
      } else {
        cat(sprintf("%8i\t%5.1E\t%5i\t%15.1E\t%9i\n", iter, res, inner_it_count, 1))
      }
    }
    
    # Start of line search
    lambda_info <- armijoStepLimiter(x, dx, F, f0)
    lambda <- lambda_info[[1]]
    armIter <- lambda_info[[2]]
    
    # Update the solution and residual
    x <- x + lambda * dx
    f0 <- F(x)
    res <- norm(f0)
    
    # Adjust eta as per Eisenstat-Walker
    gmparams[1] <- eisenstatWalkerForcing(gmparams[1], etamax, residualRatio, res, stop_tol)
    
    if (!is.null(x_hist)) {
      x_hist <- cbind(x_hist, x)
    }
    
    it_histx[iter + 1, ] <- updateIterationHistory(it_histx[iter, ], res, inner_it_count, armIter)
  }
  
  if (res > stop_tol) {
    stop("Newton failed to converge after maxiter iterations!")
  }
  
  sol <- x
  it_hist <- it_histx[1:(iter + 1), ]
  
  return(list(sol = sol, it_hist = it_hist, x_hist = x_hist))
}

# Additional helper functions below...

armijoStepLimiter <- function(x, dx, F, f0) {
  alpha <- 1e-4
  maxarm <- 20
  
  lambda <- 1
  lambda_old <- lambda
  armIter <- 0
  
  xt <- x + lambda * dx
  tempRes <- norm(F(xt))
  
  currentRes <- norm(f0)
  
  ff0 <- currentRes^2
  ffc <- tempRes^2
  ffm <- ffc
  
  while (tempRes >= (1 - alpha * lambda) * currentRes) {
    if (armIter == 0) {
      lambda <- 0.5 * lambda
    } else {
      lambda <- parab3p(lambda, lambda_old, ff0, ffc, ffm)
    }
    
    xt <- x + lambda * dx
    lambda_old <- lambda
    
    ft <- F(xt)
    tempRes <- norm(ft)
    ffm <- ffc
    ffc <- tempRes^2
    
    armIter <- armIter + 1
    if (armIter > maxarm) {
      stop("Newton failed in the line search. Too many step length reductions.")
    }
  }
  
  return(list(lambda, armIter))
}

parab3p <- function(lambdac, lambdam, ff0, ffc, ffm) {
  sigma0 <- 0.1
  sigma1 <- 0.5
  
  c2 <- lambdam * (ffc - ff0) - lambdac * (ffm - ff0)
  if (c2 >= 0) {
    return(sigma1 * lambdac)
  }
  c1 <- lambdac^2 * (ffm - ff0) - lambdam^2 * (ffc - ff0)
  lambdap <- -c1 * 0.5 / c2
  if (lambdap < sigma0 * lambdac) {
    lambdap <- sigma0 * lambdac
  }
  if (lambdap > sigma1 * lambdac) {
    lambdap <- sigma1 * lambdac
  }
  return(lambdap)
}

eisenstatWalkerForcing <- function(eta_old, eta_max, rat, fnrm, stop_tol) {
  gamma <- 0.9
  
  eta_safe <- eta_max
  
  if (eta_max > 0) {
    eta_new <- gamma * rat^2
    
    if (gamma * eta_old^2 > 0.1) {
      eta_new <- max(eta_new, gamma * eta_old^2)
    }
    
    eta_safe <- min(eta_max, eta_new)
  } else {
    stop("eta_max should be > 0")
  }
  
  eta <- min(eta_max, max(eta_safe, 0.5 * stop_tol / fnrm))
  return(eta)
}

updateIterationHistory <- function(it_hist_prev, res, inner_f_evals, iarm) {
  it_hist_update <- numeric(3)
  it_hist_update[1] <- res
  it_hist_update[2] <- it_hist_prev[2] + inner_f_evals + iarm + 1
  it_hist_update[3] <- iarm
  return(it_hist_update)
}

krylovSolve <- function(f0, f, x, params, lmeth) {
  # Initialization
  if (length(params) >= 2) {
    gmparms <- c(params[1], params[2])
  } else {
    stop("Not enough parameters passed to the Krylov Solver")
  }
  
  if (lmeth == 1 || lmeth == 2) {
    maxGMRESiters <- params[2]
    
    if (lmeth == 1) {
      restart_limit <- 0
    } else {
      restart_limit <- 20
      if (length(params) >= 3) {
        restart_limit <- params[3]
      }
    }
    
    if (length(params) == 4) {
      gmparms <- c(params[1], params[2], params[4])
    } else {
      gmparms <- c(params[1], params[2], 1)
    }
    
    dx_info <- gmres(f0, f, x, gmparms)
    restarts <- 0
    
    while (dx_info[[3]] == maxGMRESiters && 
           dx_info[[2]][maxGMRESiters] > gmparms[1] * norm(f0) && 
           restarts < restart_limit) {
      restarts <- restarts + 1
      dx_info <- gmres(f0, f, x, gmparms, dx_info[[1]])
    }
    
    total_iters <- dx_info[[3]] + restarts * maxGMRESiters
    f_evals <- total_iters + restarts
    return(list(dx_info[[1]], dx_info[[2]], total_iters))
  } else {
    stop("Krylov solver choice invalid")
  }
}

gmres <- function(f0, f, xc, params, dxinit = NULL) {
  errtol <- params[1]
  kmax <- params[2]
  
  reorth <- 1
  if (length(params) == 3) {
    reorth <- params[3]
  }
  
  n <- length(f0)
  dx <- numeric(n)
  
  b <- -f0
  r <- b
  if (!is.null(dxinit))
    