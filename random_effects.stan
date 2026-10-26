data {
  int<lower=1> number_of_countries;
  int<lower=1> number_of_country_years;
  int<lower=1> number_of_outcome_observations;

  int<lower=1> number_of_instruments;
  int<lower=1> number_of_endogenous_variables;
  int<lower=1> number_of_controls;
  
  matrix[number_of_country_years, number_of_instruments] instruments;
  matrix[number_of_country_years, number_of_endogenous_variables] endogenous_variables;
  matrix[number_of_country_years, number_of_controls] controls;

  array[number_of_country_years] int<lower=1, upper=number_of_countries> 
    country_index_of_country_year;

  vector[number_of_outcome_observations] outcomes;
  array[number_of_outcome_observations] 
    int<lower=1, upper=number_of_country_years> 
    country_year_index_of_observation;
}

transformed data {
  int number_of_stages = number_of_endogenous_variables + 1;
}

parameters {
  row_vector[number_of_stages] intercepts;
  matrix[number_of_controls, number_of_stages] control_coefficients;
  matrix[number_of_instruments, number_of_endogenous_variables] instrument_coefficients;
  vector[number_of_endogenous_variables] endogenous_variable_coefficients;

  matrix[number_of_countries, number_of_stages] z_country_effects;
  row_vector<lower=0>[number_of_stages] country_effects_standard_deviations;
  cholesky_factor_corr[number_of_stages] lower_country_effects_correlation_matrix;

  // non-centered: final stage residuals are built from these in the model block
  vector[number_of_country_years] z_final_stage_residuals;

  row_vector<lower=0>[number_of_stages] residual_standard_deviations;
  cholesky_factor_corr[number_of_stages] lower_residual_correlation_matrix;
  
  real<lower=0> outcome_standard_deviation;
}

model {
  intercepts ~ std_normal();
  to_vector(control_coefficients) ~ std_normal();
  to_vector(instrument_coefficients) ~ std_normal();
  endogenous_variable_coefficients ~ std_normal();

  lower_country_effects_correlation_matrix ~ lkj_corr_cholesky(2);

  for (country_index in 1:number_of_countries) {
    z_country_effects[country_index, :] ~
      multi_normal_cholesky(
        rep_vector(0, number_of_stages),
        lower_country_effects_correlation_matrix
      );
  }

  country_effects_standard_deviations ~ exponential(1);

  matrix[number_of_country_years, number_of_stages] predictions =
    rep_matrix(intercepts, number_of_country_years) +
    instruments * append_col(
      instrument_coefficients,
      rep_vector(0, number_of_instruments)
    ) +
    endogenous_variables * append_col(
      rep_matrix(0, number_of_endogenous_variables, number_of_endogenous_variables),
      endogenous_variable_coefficients
    ) +
    controls * control_coefficients +
    diag_post_multiply(
      z_country_effects,
      country_effects_standard_deviations
    )[country_index_of_country_year, :];

  residual_standard_deviations ~ exponential(1);
  lower_residual_correlation_matrix ~ lkj_corr_cholesky(2);

  // residuals for all stages are jointly multivariate normal
  matrix[number_of_stages, number_of_stages] residual_cholesky =
    diag_pre_multiply(
      residual_standard_deviations,
      lower_residual_correlation_matrix
    );

  matrix[number_of_endogenous_variables, number_of_endogenous_variables]
    first_stage_cholesky = residual_cholesky[
      1:number_of_endogenous_variables,
      1:number_of_endogenous_variables
    ];

  matrix[number_of_country_years, number_of_endogenous_variables]
    first_stage_residuals =
    endogenous_variables - predictions[:, 1:number_of_endogenous_variables];

  for (country_year_index in 1:number_of_country_years) {
    first_stage_residuals[country_year_index, :] ~
      multi_normal_cholesky(
        rep_vector(0, number_of_endogenous_variables),
        first_stage_cholesky
      );
  }

  // the final stage residuals, conditional on the first stage residuals,
  // so that the residuals for all stages have the joint distribution above
  // residuals = residual_cholesky * standard normals, so solve for the
  // first stage standard normals
  matrix[number_of_endogenous_variables, number_of_country_years]
    z_first_stage_residuals =
    mdivide_left_tri_low(first_stage_cholesky, first_stage_residuals');

  z_final_stage_residuals ~ std_normal();

  vector[number_of_country_years] final_stage_residuals =
    (
      residual_cholesky[number_of_stages, 1:number_of_endogenous_variables] *
        z_first_stage_residuals
    )' +
    residual_cholesky[number_of_stages, number_of_stages] *
      z_final_stage_residuals;

  outcome_standard_deviation ~ exponential(1);
  outcomes ~ normal(
    (predictions[:, number_of_stages] + final_stage_residuals)
      [country_year_index_of_observation],
    outcome_standard_deviation
  );
}
