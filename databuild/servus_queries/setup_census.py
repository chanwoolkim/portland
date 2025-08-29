# Set up
API_KEY = os.getenv('CENSUS_API_KEY')
client = Census(API_KEY)
state_fips = states.OR.fips
counties = ['051', '067', '005']  # Clackamas, Washington, Multnomah (FIPS codes)

# Variables and labels
demographic_stats = {
  'total_house_holds': 'B11001_001E',
  'average_house_hold_size': 'B25010_001E',
  'average_house_hold_size_owner': 'B25010_002E',
  'average_house_hold_size_renter': 'B25010_003E',
  'educational_attainment_total': 'B15003_001E',
  'educational_attainment_no_schooling': 'B15003_002E',
  'educational_attainment_nursery_school': 'B15003_003E',
  'educational_attainment_kindergarten': 'B15003_004E',
  'educational_attainment_1st_grade': 'B15003_005E',
  'educational_attainment_2nd_grade': 'B15003_006E',
  'educational_attainment_3rd_grade': 'B15003_007E',
  'educational_attainment_4th_grade': 'B15003_008E',
  'educational_attainment_5th_grade': 'B15003_009E',
  'educational_attainment_6th_grade': 'B15003_010E',
  'educational_attainment_7th_grade': 'B15003_011E',
  'educational_attainment_8th_grade': 'B15003_012E',
  'educational_attainment_9th_grade': 'B15003_013E',
  'educational_attainment_10th_grade': 'B15003_014E',
  'educational_attainment_11th_grade': 'B15003_015E',
  'educational_attainment_12th_grade_no_diploma': 'B15003_016E',
  'educational_attainment_high_school_diploma': 'B15003_017E',
  'educational_attainment_ged_or_alternative': 'B15003_018E',
  'educational_attainment_some_college_less_than_1_year': 'B15003_019E',
  'educational_attainment_some_college_1_or_more_years_no_degree': 'B15003_020E',
  'educational_attainment_associates_degree': 'B15003_021E',
  'educational_attainment_bachelors_degree': 'B15003_022E',
  'educational_attainment_masters_degree': 'B15003_023E',
  'educational_attainment_professional_school_degree': 'B15003_024E',
  'educational_attainment_doctorate_degree': 'B15003_025E',
  'aggregate_house_hold_income': 'B19025_001E',
  'aggregate_house_hold_earnings': 'B19061_001E',
  'aggregate_house_hold_retirement_income': 'B19069_001E',
  'aggregate_house_hold_cash_assistance': 'B19067_001E',
  'median_house_hold_income': 'B19013_001E',
  'house_holds_with_under_18': 'B11008_003E',
  'house_holds_with_65_plus': 'B11007_002E',
  'house_holds_with_retirement_income': 'B19059_002E',
  'house_holds_with_ssi': 'B19055_002E',
  'house_holds_with_cash_assistance': 'B19057_002E',
  'house_holds_with_food_stamps': 'B19058_002E',
  'house_holds_in_poverty': 'B17017_002E',
  'employment_total': 'B23025_002E',
  'employed_total': 'B23025_004E',
  'unemployed_total': 'B23025_005E',
  'total_population_over_16':'B23025_001E',
  'male_under_6_with_no_health_insurance': 'B27001_005E',
  'male_6_to_18_with_no_health_insurance': 'B27001_008E',
  'male_19_to_25_with_no_health_insurance': 'B27001_011E',
  'male_26_to_34_with_no_health_insurance': 'B27001_014E',
  'male_35_to_44_with_no_health_insurance': 'B27001_017E',
  'male_45_to_54_with_no_health_insurance': 'B27001_020E',
  'male_55_to_64_with_no_health_insurance': 'B27001_023E',
  'male_65_to_74_with_no_health_insurance': 'B27001_026E',
  'male_over_75_with_no_health_insurance': 'B27001_029E',
  'female_under_6_with_no_health_insurance': 'B27001_033E',
  'female_6_to_18_with_no_health_insurance': 'B27001_036E',
  'female_19_to_25_with_no_health_insurance': 'B27001_039E',
  'female_26_to_34_with_no_health_insurance': 'B27001_042E',
  'female_35_to_44_with_no_health_insurance': 'B27001_045E',
  'female_45_to_54_with_no_health_insurance': 'B27001_048E',
  'female_55_to_64_with_no_health_insurance': 'B27001_051E',
  'female_65_to_74_with_no_health_insurance': 'B27001_054E',
  'female_over_75_with_no_health_insurance': 'B27001_057E',
  'median_number_of_rooms': 'B25018_001E',
  'vehicles_owned_0': 'B25044_003E',
  'vehicles_owned_1': 'B25044_004E',
  'vehicles_owned_2': 'B25044_005E',
  'vehicles_owned_3': 'B25044_006E',
  'vehicles_owned_4': 'B25044_007E',
  'vehicles_owned_more_than_4': 'B25044_008E',
  'housing_cost_renter': 'B25064_001E',
  'housing_cost_owner': 'B25088_002E',
  'house_hold_renting': 'B25033_008E',
  'house_hold_owning': 'B25033_002E',
  'median_housing_market_value': 'B25077_001E',
  'median_age': 'B01002_001E',
  'male_age_under_5': 'B01001_003E',
  'male_age_5_to_9': 'B01001_004E',
  'male_age_10_to_14': 'B01001_005E',
  'male_age_15_to_17': 'B01001_006E',
  'male_age_18_to_19': 'B01001_007E',
  'male_age_20': 'B01001_008E',
  'male_age_21': 'B01001_009E',
  'male_age_22_to_24': 'B01001_010E',
  'male_age_25_to_29': 'B01001_011E',
  'male_age_30_to_34': 'B01001_012E',
  'male_age_35_to_39': 'B01001_013E',
  'male_age_40_to_44': 'B01001_014E',
  'male_age_45_to_49': 'B01001_015E',
  'male_age_50_to_54': 'B01001_016E',
  'male_age_55_to_59': 'B01001_017E',
  'male_age_60_to_61': 'B01001_018E',
  'male_age_62_to_64': 'B01001_019E',
  'male_age_65_to_66': 'B01001_020E',
  'male_age_67_to_69': 'B01001_021E',
  'male_age_70_to_74': 'B01001_022E',
  'male_age_75_to_79': 'B01001_023E',
  'male_age_80_to_84': 'B01001_024E',
  'male_age_over_85': 'B01001_025E',
  'female_age_under_5': 'B01001_027E',
  'female_age_5_to_9': 'B01001_028E',
  'female_age_10_to_14': 'B01001_029E',
  'female_age_15_to_17': 'B01001_030E',
  'female_age_18_to_19': 'B01001_031E',
  'female_age_20': 'B01001_032E',
  'female_age_21': 'B01001_033E',
  'female_age_22_to_24': 'B01001_034E',
  'female_age_25_to_29': 'B01001_035E',
  'female_age_30_to_34': 'B01001_036E',
  'female_age_35_to_39': 'B01001_037E',
  'female_age_40_to_44': 'B01001_038E',
  'female_age_45_to_49': 'B01001_039E',
  'female_age_50_to_54': 'B01001_040E',
  'female_age_55_to_59': 'B01001_041E',
  'female_age_60_to_61': 'B01001_042E',
  'female_age_62_to_64': 'B01001_043E',
  'female_age_65_to_66': 'B01001_044E',
  'female_age_67_to_69': 'B01001_045E',
  'female_age_70_to_74': 'B01001_046E',
  'female_age_75_to_79': 'B01001_047E',
  'female_age_80_to_84': 'B01001_048E',
  'female_age_over_85': 'B01001_049E',
  'population_includes_native_american': 'B02001_004E',
  'population_includes_hispanic': 'B03003_003E',
  'population_includes_asian': 'B02001_005E',
  'population_includes_pacific_islander': 'B02001_006E',
  'population_includes_black': 'B02001_003E',
  'population_includes_white': 'B02001_002E',
  'population_includes_other_race': 'B02001_007E'
}

demographic_stats_list = list(demographic_stats.values())

# Get ACS data per county
acs_data = []
for year in range(2019, 2024):
  for county_fips in counties:
      result = client.acs5.get(demographic_stats_list,
                          {'for': 'block group:*',
                          'in': f'state:{state_fips} county:{county_fips} tract:*'
                          },
                          year=year)
      for row in result:
        row['year'] = year
      acs_data.extend(result)

acs_df = pd.DataFrame(acs_data)
acs_code_to_name = {v: k for k, v in demographic_stats.items()}

# Add state, county, and tract columns
id_vars = ['state', 'county', 'tract', 'block group', 'year']
acs_df = acs_df[[*acs_code_to_name.keys(), *id_vars]]

acs_df['census_year'] = acs_df['year'].astype(int)
acs_df['state_id'] = acs_df['state'].astype(int)
acs_df['county_id'] = acs_df['county'].astype(int)
acs_df['tract_id'] = acs_df['tract'].astype(int)
acs_df['block_group_id'] = acs_df['block group'].astype(int)

# Rename the columns
acs_df.rename(columns=acs_code_to_name, inplace=True)

# Save data
acs_df.to_parquet(os.path.join(working_data_dir, 'pre-processed', 'raw_census_data.parquet'), index=False)
