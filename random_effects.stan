data {
  int<lower=1> number_of_country_years;
  int<lower=1> number_of_outcome_observations;
  int<lower=1> number_of_controls;
  int<lower=1> number_of_countries;
  int<lower=1> number_of_years;

  vector[number_of_country_years] instrument;
  vector[number_of_country_years] endogenous_variable;
  
  matrix[number_of_country_years, number_of_controls] controls;
  array[number_of_country_years] int<lower=1, upper=number_of_countries> 
    country_index_of_country_year;
  array[number_of_country_years] int<lower=1, upper=number_of_years> 
    year_index_of_country_year;

  vector[number_of_outcome_observations] outcomes;
  array[number_of_outcome_observations] 
    int<lower=1, upper=number_of_country_years> 
    country_year_index_of_observation;
}

transformed data {
  matrix[number_of_country_years, 2] main_variables = 
    append_col(instrument, endogenous_variable);
}

parameters {
  row_vector[2] intercepts;
  row_vector[2] main_coefficients;
  matrix[number_of_controls, 2] control_coefficients;

  sum_to_zero_vector[number_of_years] stage_1_year_effects;
  sum_to_zero_vector[number_of_years] stage_2_year_effects;

  sum_to_zero_vector[number_of_countries] stage_1_z_country_effects;
  sum_to_zero_vector[number_of_countries] stage_2_z_country_effects;
  row_vector<lower=0>[2] country_effects_standard_deviations;
  cholesky_factor_corr[2] lower_random_effects_correlation_matrix;

  vector[number_of_country_years] outcome_mean_errors;
  
  row_vector<lower=0>[2] residual_standard_deviations;
  cholesky_factor_corr[2] lower_residual_correlation_matrix;
  
  real<lower=0> outcome_standard_deviation;
}

model {
  intercepts ~ std_normal();
  main_coefficients ~ std_normal();
  to_vector(control_coefficients) ~ std_normal();

  stage_1_year_effects ~ std_normal();
  stage_2_year_effects ~ std_normal();

  matrix[number_of_countries, 2] z_country_effects = 
    append_col(
      stage_1_z_country_effects,
      stage_2_z_country_effects
    );

  country_effects_standard_deviations ~ exponential(1);
  lower_random_effects_correlation_matrix ~ lkj_corr_cholesky(2);

  for (country_index in 1:number_of_countries) {
    z_country_effects[country_index, :] ~
      multi_normal_cholesky(
        [0, 0],
        lower_random_effects_correlation_matrix
      );
  }

  matrix[number_of_country_years, 2] predictions =
    rep_matrix(intercepts, number_of_country_years) +
    diag_post_multiply(main_variables, main_coefficients) +
    controls * control_coefficients +
    append_col(stage_1_year_effects, stage_2_year_effects)[
      year_index_of_country_year, :
    ] +
    diag_post_multiply(
      z_country_effects,
      country_effects_standard_deviations
    )[country_index_of_country_year, :];

  matrix[number_of_country_years, 2] errors = 
    append_col(
      endogenous_variable - predictions[:, 1],
      outcome_mean_errors
    );
  
  residual_standard_deviations ~ exponential(1);
  lower_residual_correlation_matrix ~ lkj_corr_cholesky(2);

  for (country_year_index in 1:number_of_country_years) {
    errors[country_year_index, :] ~ 
      multi_normal_cholesky(
        predictions[country_year_index, :],
        diag_pre_multiply(
          residual_standard_deviations,
          lower_residual_correlation_matrix
        )
      );
  }

  outcome_standard_deviation ~ exponential(1);
  outcomes ~ normal(
    (predictions[:, 2] + outcome_mean_errors)
      [country_year_index_of_observation],
    outcome_standard_deviation
  );
}
