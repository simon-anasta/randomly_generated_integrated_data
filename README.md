# Randomly generated integrated data
Exploring the creation of a randomly generated data that mimics parts of the IDI.

### Goal
For learning and training purposes it would be useful for researchers to have access to a data set with some of the properties of the Integrated Data Infrastructure (IDI, by Stats NZ). There is no need for such a dataset to contain actual data, nor for the randomly generated data to be very close to the truth. Hence I am creating this data source by randomly generating data.

Given my experience working with the IDI, I have some idea of the general patterns that are found in the data (for example, where two tables should contain consistent information). This randomly generated dataset contains some of the patterns that I think might be relevant for researchers to encounter when practicing.

### Dataset use
This dataset is artificial, it was randomly generated for the purposes of training and practice. The dataset is most suited to teaching data wrangling skills and helping IDI researchers prepare.

No results generated using this randomly generated data should be used for any purpose beyond teaching, training, or practice. Any results generated from this data are have no meaning. No decisions should be based on this data. No inference or conclusions should be drawn (this is what is means for the data to be randomly generated).

In some cases we have tried to match known patterns in New Zealand (for example, the life expectancy). This is to make learning easier for trainees by having the data match **some** of their intuition. Anyone interested in the underlying patterns in New Zealand should consult official sources.

### Code execution
The repository currently only stores a small amount of randomly generated example data (about 1,000 and 10,000 people). For training purposes a much larger dataset may be prefered (for example 1,000,000 people). As datasets of any size can be randomly generated on demand, we have decided to **not** store very large datasets in this repository.

Each R script is responsible for the random generation of a single part of the practice dataset. Settings for each part are found at the top of each script. These include:
* The random seed (for repeatibility)
* Population size
* Probabilities of certain events happening
* Minimum and maximum values for certain ranges

Other parameters are stored in the Lookups subfolder. This contains a collection of csv files with values that are used for random generation. Teachers wanting to create a custom dataset are welcome to adjust the parameters either in the R scripts or in the csv lookup tables. However, some caution is advised as large edits may break the generation code.

Many scripts are dependent on other scripts alreadying being run. For example, we can not generate income from employment data without first generating employer data. We recommend the scripts in the Code main subfolder are run in the following order:

1. generate_personal_detail.R
2. generate_snz_res_pop.R
3. generate_person_overseas_spell.R
4. generate_employers.R
5. generate_ird_ems.R
6. generate_pub_fund_hosp_discharges_event.R
7. generate_pub_fund_hosp_discharges_diag.R
8. generate_drivers_licence_register_historic.R
9. add_linking_error.R

With the last file being optional, depending on whether you want your data to contain some random errors.

Note that while most files generate quickly, generating the IRD EMS table is a slow process. This table is very large, containing one row per person per month. For generating random data for 100,000 people or more you will likely want to run this script overnight.

### Wrangling practice
Most analysis and research requires data to be prepared in a single rectangular table: one row per person, one column per measurement (age, income, etc.). Data wrangling is an important step to go from many separate tables to a single research-ready table.

The following is a list of measures that can be assembled / wrangled from the randomly generated data:

* age
* ethnicity
* sex
* residence in New Zealand
* ever resident
* days overseas
* ever travelled overseas
* born overseas
* is employed
* income
* change in income from last year
* types of income
* received government income support
* change employer
* region of employment
* hospital in-patient 
* days in hospital
* hospital diagnosis by ICD9 or ICD10 code
* transfer between hospitals
* parents' characteristics (age, ethnicity, income, etc.)
* employer's characteristics (size, turnover, etc.)
* has a drivers licence, by type
* had a licence but it is currently suspended or unavailable

The tables in this collection all have slightly different formats that make the dataset suitable for wrangling practice:

* [data]_[personal_detail] is a cross-sectional table
* [data]_[snz_res_pop] is a time series table
* [data]_[person_overseas_spell] is a spell table with start and end dates
* [ir_clean]_[ird_ems] has detailed time series, alloing for concurrent occurrences
* [moh_clean]_[pub_fund_hosp_discharges] is an event table with one-to-many lookup
* [nzta_clean]_[drivers_licence_register] has both a current state table and a simplified history table
