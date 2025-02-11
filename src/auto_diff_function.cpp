
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
  DATA_VECTOR(x);
  PARAMETER_VECTOR(y);
  Type nll = 0.0;
  vector<Type> z = x * y;
  ADREPORT(z);
  return nll;
}

