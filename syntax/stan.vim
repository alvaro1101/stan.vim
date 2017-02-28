" Vim syntax file
" Language:	Stan (http://mc-stan.org)
" Maintainer:	J. Guo <guojq28@gmail.com> 
" Last Change:  Aug 7 2016
" Filenames:	*.stan
" URL:		

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if version >= 600
  setlocal iskeyword=@,48-57,_,.
else
  set iskeyword=@,48-57,_,.
endif

syn case match

syntax match stanCommentError display "\*/" 
syntax match stanCommentStartError display "/\*"me=e-1 contained 

syn keyword	stanTodo	TODO FIXME TBD contained 
syn cluster	stanCommentGroup	contains=stanTodo

" Comment
" copied from c.vim, not exactly know what is going on
syn region stanCommentL start="//" skip="\\$" end="$" keepend contains=@stanCommentGroup,@Spell 
syn match  stanCommentL /\#.*/ 
" syn region stanComment start="/\*" end="\*/" contains=stanTodo 
syn region stanComment start="/\*" end="\*/" contains=@stanCommentGroup,@stanCommentError,@Spell extend 

" Constant
" string enclosed in double quotes
syn region stanString start=/"/ skip=/\\\\\|\\"/ end=/"/
" string enclosed in single quotes
syn region stanString start=/'/ skip=/\\\\\|\\'/ end=/'/
" number with no fractional part or exponent
syn match stanNumber /\d\+/
" floating point number with integer and fractional parts and optional exponent
syn match stanFloat /\d\+\.\d*\([Ee][-+]\=\d\+\)\=/
" floating point number with no integer part and optional exponent
syn match stanFloat /\.\d\+\([Ee][-+]\=\d\+\)\=/
" floating point number with no fractional part and optional exponent
syn match stanFloat /\d\+[Ee][-+]\=\d\+/

" Identifier
" identifier with leading letter and optional following keyword characters
syn match stanIdentifier /\a\k*/
" identifier with leading period, one or more digits, and at least one non-digit keyword character
syn match stanIdentifier /\.\d*\K\k*/

" Statement
" syn keyword stanStatement   break next return
syn keyword stanConditional if else
syn keyword stanRepeat      for in while

" Constant
" syn keyword stanConstant LETTERS letters month.ab month.name pi
" syn keyword stanConstant NULL
" syn keyword stanBoolean  FALSE TRUE
" syn keyword stanNumber   NA
syn match stanArrow /<\{1}-/
syn match stanDistributed /\~/ 

" Type
" syn keyword stanType data model  array category character complex real function integer list logical matrix numeric vector data.frame 
" syn keyword stanType var 

syn keyword stanType int real vector row_vector matrix unit_vector simplex ordered positive_ordered cholesky_factor_cov cholesky_factor_corr cov_matrix corr_matrix
syn keyword stanBlk functions data model parameters transformed generated quantities 
" syn keyword stanBlk derived
syn match equalSign /=/
syn keyword stanLU lower upper nextgroup=equalSign skipwhite

syn match leftParen /(/
syn keyword stanDistributions  bernoulli bernoulli_logit beta_binomial binomial binomial_logit categorical categorical_logit cauchy chi_square dirichlet double_exponential exp_mod_normal exponential frechet gaussian_dlm_obs gumbel hypergeometric inv_chi_square inv_gamma inv_wishart lkj_corr_cholesky lkj_corr logistic lognormal multi_gp multi_gp_cholesky multi_normal_cholesky multi_normal multi_normal_prec multi_student_t multinomial neg_binomial neg_binomial_2 neg_binomial_2_log normal ordered_logistic pareto pareto_type_2 poisson poisson_log rayleigh scaled_inv_chi_square skew_normal student_t uniform von_mises weibull wiener wishart
" only highight gamma beta such that there is '(' after
" that indicating it is distribution.
" For the time being, not define others. `normal` should 
" not be a name used. 
syn match stanDistributionsII  /gamma\(\s*(\)\@=/
syn match stanDistributionsII  /beta\(\s*(\)\@=/


syn keyword stanFunctions beta_lpdf cauchy_lpdf chi_square_lpdf dirichlet_lpdf double_exponential_lpdf exp_mod_normal_lpdf exponential_lpdf frechet_lpdf gamma_lpdf gaussian_dlm_obs_lpdf gumbel_lpdf inv_chi_square_lpdf inv_gamma_lpdf inv_wishart_lpdf lkj_corr_cholesky_lpdf lkj_corr_lpdf logistic_lpdf lognormal_lpdf multi_gp_lpdf multi_gp_cholesky_lpdf multi_normal_cholesky_lpdf multi_normal_lpdf multi_normal_prec_lpdf multi_student_t_lpdf normal_lpdf pareto_lpdf pareto_type_2_lpdf rayleigh_lpdf scaled_inv_chi_square_lpdf skew_normal_lpdf student_t_lpdf uniform_lpdf von_mises_lpdf weibull_lpdf wiener_lpdf wishart_lpdf

syn keyword stanFunctions bernoulli_lpmf bernoulli_logit_lpmf beta_binomial_lpmf binomial_lpmf binomial_logit_lpmf categorical_lpmf categorical_logit_lpmf hypergeometric_lpmf multinomial_lpmf neg_binomial_lpmf neg_binomial_2_lpmf neg_binomial_2_log_lpmf ordered_logistic_lpmf poisson_lpmf poisson_log_lpmf

syn keyword stanFunctions bernoulli_cdf bernoulli_lccdf bernoulli_lcdf beta_binomial_cdf beta_binomial_lccdf beta_binomial_lcdf beta_cdf beta_lccdf beta_lcdf binomial_cdf binomial_lccdf binomial_lcdf cauchy_cdf cauchy_lccdf cauchy_lcdf chi_square_cdf chi_square_lccdf chi_square_lcdf double_exponential_cdf double_exponential_lccdf double_exponential_lcdf exp_mod_normal_cdf exp_mod_normal_lccdf exp_mod_normal_lcdf exponential_cdf exponential_lccdf exponential_lcdf frechet_cdf frechet_lccdf frechet_lcdf gamma_cdf gamma_lccdf gamma_lcdf gumbel_cdf gumbel_lccdf gumbel_lcdf inv_chi_square_cdf inv_chi_square_lccdf inv_chi_square_lcdf inv_gamma_cdf inv_gamma_lccdf inv_gamma_lcdf logistic_cdf logistic_lccdf logistic_lcdf lognormal_cdf lognormal_lccdf lognormal_lcdf neg_binomial_cdf neg_binomial_lccdf neg_binomial_lcdf neg_binomial_2_cdf neg_binomial_2_lccdf neg_binomial_2_lcdf normal_cdf normal_lccdf normal_lcdf pareto_cdf pareto_lccdf pareto_lcdf pareto_type_2_cdf pareto_type_2_lccdf pareto_type_2_lcdf poisson_cdf poisson_lccdf poisson_lcdf rayleigh_cdf rayleigh_lccdf rayleigh_lcdf scaled_inv_chi_square_cdf scaled_inv_chi_square_lccdf scaled_inv_chi_square_lcdf skew_normal_cdf skew_normal_lccdf skew_normal_lcdf student_t_cdf student_t_lccdf student_t_lcdf uniform_cdf uniform_lccdf uniform_lcdf weibull_cdf weibull_lccdf weibull_lcdf

syn keyword stanFunctions bernoulli_lcdf beta_binomial_lcdf beta_lcdf binomial_lcdf cauchy_lcdf chi_square_lcdf double_exponential_lcdf exp_mod_normal_lcdf exponential_lcdf frechet_lcdf gamma_lcdf gumbel_lcdf inv_chi_square_lcdf inv_gamma_lcdf logistic_lcdf lognormal_lcdf neg_binomial_lcdf neg_binomial_2_lcdf normal_lcdf pareto_lcdf pareto_type_2_lcdf poisson_lcdf rayleigh_lcdf scaled_inv_chi_square_lcdf skew_normal_lcdf student_t_lcdf uniform_lcdf weibull_lcdf

syn keyword stanFunctions bernoulli_lccdf beta_binomial_lccdf beta_lccdf binomial_lccdf cauchy_lccdf chi_square_lccdf double_exponential_lccdf exp_mod_normal_lccdf exponential_lccdf frechet_lccdf gamma_lccdf gumbel_lccdf inv_chi_square_lccdf inv_gamma_lccdf logistic_lccdf lognormal_lccdf neg_binomial_lccdf neg_binomial_2_lccdf normal_lccdf pareto_lccdf pareto_type_2_lccdf poisson_lccdf rayleigh_lccdf scaled_inv_chi_square_lccdf skew_normal_lccdf student_t_lccdf uniform_lccdf weibull_lccdf

syn keyword stanFunctions bernoulli_rng bernoulli_logit_rng beta_binomial_rng beta_rng binomial_rng categorical_rng cauchy_rng chi_square_rng dirichlet_rng double_exponential_rng exp_mod_normal_rng exponential_rng frechet_rng gamma_rng gumbel_rng hypergeometric_rng inv_chi_square_rng inv_gamma_rng inv_wishart_rng lkj_corr_cholesky_rng lkj_corr_rng logistic_rng lognormal_rng multi_normal_rng multi_normal_cholesky_rng multi_student_t_rng multinomial_rng neg_binomial_rng neg_binomial_2_rng neg_binomial_2_log_rng normal_rng ordered_logistic_rng pareto_rng pareto_type_2_rng poisson_rng poisson_log_rng rayleigh_rng scaled_inv_chi_square_rng skew_normal_rng student_t_rng uniform_rng von_mises_rng weibull_rng wishart_rng

syn keyword stanFunctions abs acos acosh add asin asinh atan atan2 atanh bessel_first_kind bessel_second_kind binary_log_loss block append_col cbrt ceil cholesky_decompose choose col cols columns_dot_product columns_dot_self cos cosh cov_exp_quad crossprod csr_matrix_times_vector csr_to_dense_matrix csr_extract_w csr_extract_v csr_extract_u cumulative_sum determinant diag_matrix diag_post_multiply diag_pre_multiply diagonal digamma dims distance divide dot_product dot_self e eigenvalues_sym eigenvectors_sym qr_Q qr_R elt_divide elt_multiply erf erfc exp exp2 expm1 fabs falling_factorial fdim floor fma fmax fmin fmod gamma_p gamma_q get_lp head hypot if_else inc_beta int_step inv inv_cloglog inv_logit inv_Phi inv_sqrt inv_square inverse inverse_spd is_inf is_nan lbeta lchoose lgamma lmgamma lmultiply log log10 log1m log1m_exp log1m_inv_logit log1p log1p_exp log2 log_determinant log_diff_exp log_falling_factorial log_mix log_rising_factorial log_inv_logit log_softmax log_sum_exp logical_negation logical_or logical_and logical_eq logical_neq logical_lt logical_lte logical_gt logical_gte logit machine_precision matrix_exp max mdivide_left mdivide_left_spd mdivide_left_tri_low mdivide_right mdivide_right_spd mdivide_right_tri_low mean min minus modified_bessel_first_kind modified_bessel_second_kind modulus multiply multiply_lower_tri_self_transpose negative_infinity not_a_number num_elements owens_t Phi Phi_approx pi positive_infinity pow prod quad_form quad_form_sym quad_form_diag rank append_row rep_array rep_matrix rep_row_vector rep_vector rising_factorial round row rows rows_dot_product rows_dot_self sd segment sin singular_values sinh size softmax sort_asc sort_desc sort_indices_asc sort_indices_desc squared_distance sqrt sqrt2 square step sub_col sub_row subtract sum tail tan tanh target tcrossprod tgamma to_array_1d to_array_2d to_matrix to_row_vector to_vector trace trace_gen_quad_form trace_quad_form transpose trunc trigamma variance

" Special
syn match stanDelimiter /[,;:><]/

" Error
syn region stanRegion matchgroup=Delimiter start=/(/ matchgroup=Delimiter end=/)/ transparent contains=ALLBUT,stanError,stanBraceError,stanCurlyError
syn region stanRegion matchgroup=Delimiter start=/{/ matchgroup=Delimiter end=/}/ transparent contains=ALLBUT,stanError,stanBraceError,stanParenError
syn region stanRegion matchgroup=Delimiter start=/\[/ matchgroup=Delimiter end=/]/ transparent contains=ALLBUT,stanError,stanCurlyError,stanParenError
syn match stanError      /[)\]}]/
syn match stanBraceError /[)}]/ contained
syn match stanCurlyError /[)\]]/ contained
syn match stanParenError /[\]}]/ contained
syn match stanDAError  /<\{2,}-/
" syn match stanEqError  /\(lower\s*\|upper\s*\|>\|<\)\@<!=/
syn match stanCmp      /==/
syn match stanCmp      /!=/
syn match stanCmp      />=/
syn match stanCmp      /<=/
syn match stanCmp      />/
syn match stanCmp      /</
" transpose 
syn match stanCmp      /'/

syntax keyword stanCppConflict  alignas alignof and and_eq asm auto bitand bitor bool break case catch char
syntax keyword stanCppConflict  char16_t char32_t class compl const constexpr const_cast continue decltype
syntax keyword stanCppConflict  default delete do double dynamic_cast enum explicit export extern false
syntax keyword stanCppConflict  float friend goto inline long mutable namespace new noexcept not not_eq
syntax keyword stanCppConflict  nullptr operator or or_eq private protected public register reinterpret_cast
syntax keyword stanCppConflict  short signed sizeof static static_assert static_cast struct switch
syntax keyword stanCppConflict  template this thread_local throw true try typedef typeid typename union
syntax keyword stanCppConflict  unsigned using virtual volatile wchar_t xor xor_eq

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_r_syn_inits")
  if version < 508
    let did_r_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  " copied from sas.vim 
  hi sComment term=bold cterm=NONE ctermfg=Blue  ctermbg=Black gui=NONE guifg=DarkGreen guibg=White 

  HiLink stanComment     Comment
  HiLink stanCommentL    Comment
  HiLink stanConstant    Constant
  HiLink stanString      String
  HiLink stanNumber      Number
  HiLink stanBoolean     Boolean
  HiLink stanFloat       Float
  HiLink stanStatement   Statement
  HiLink stanConditional Conditional
  HiLink stanRepeat      Repeat
  HiLink stanIdentifier  Normal
  HiLink stanArrow	 Statement	
  HiLink stanDistributed Statement 
  HiLink stanType        Type
  HiLink stanFunctions   Function 
  HiLink stanDistributions Type 
  HiLink stanDistributionsII Type 
  HiLink stanDelimiter   Delimiter
  HiLink stanError       Error
  HiLink stanBraceError  Error
  HiLink stanCurlyError  Error
  HiLink stanParenError  Error
  HiLink stanEqError     Error
  HiLink stanDaError     Error
  HiLink stanCppConflict Error
  HiLink stanBlk         Special 
  HiLink stanLU          Special 
  HiLink stanCmp         Operator
  HiLink equalSign  Operator
  delcommand HiLink
endif

let b:current_syntax="stan"

" vim: ts=8 sw=2
" If more fancy colors (or modes) are needed, see sas.vim 
" for examples. 
