# ------------------------------------
# Function to encode non-numerical variables
# ------------------------------------
categorical_to_numerical <- function(data, map) {
    n <- c()
    for (val in data) {
        if (val %in% names(map)) {
            n <- append(n, map[val])
        }
    }
    return(n)
}

# ------------------------------------
# Function to replace missing values by certain value
# ------------------------------------
replace_missing_data <- function(data, map, value) {
    n <- c()
    for (val in data) {
        if (val %in% names(map)) {
            n <- append(n, map[val])
        } else {
            n <- append(n, value)
        }
    }
    return(n)
}

# ------------------------------------
# Function to get info collected from a boxplot
# ------------------------------------
box_data <- function(data) {
    f <- fivenum(data)
    fivenumber <- cbind(f[1], f[2], f[3], f[4], f[5])
    colnames(fivenumber) <- c("Min", "Lower-hinge", "Median", "Upper-hinge", "Max") #nolint
    return(fivenumber[1, ])
}

# ------------------------------------
# Function to calculate mode of a given variable
# ------------------------------------
mode <- function(x) {
    uniqv <- unique(x)
    t <- tabulate(match(x, uniqv))
    return(uniqv[which.max(t)])
}


# ------------------------------------
# This vectors act like mapper from categorical data
# to their numerical encoded value
# ------------------------------------
workclass <- seq(1, 8)
names(workclass) <- c(" Private", " Self-emp-not-inc", " Self-emp-inc", " Federal-gov", " Local-gov", " State-gov", " Without-pay", " Never-worked") # nolint

education <- seq(1, 16)
names(education) <- c(" Preschool", " 1st-4th", " 5th-6th", " 7th-8th", " 9th", " 10th", " 11th", " 12th", " HS-grad", " Some-college", " Assoc-voc", " Assoc-acdm", " Bachelors",  "Masters", " Prof-school", "Doctorate") # nolint

marital_status <- seq(1, 7)
names(marital_status) <- c(" Married-civ-spouse", " Divorced", " Never-married", " Separated", " Widowed", " Married-spouse-absent", " Married-AF-spouse") # nolint

occupation <- seq(1, 14)
names(occupation) <- c(" Tech-support", " Craft-repair", " Other-service", " Sales", " Exec-managerial", " Prof-specialty", " Handlers-cleaners", " Machine-op-inspct", " Adm-clerical", " Farming-fishing", " Transport-moving", " Priv-house-serv", " Protective-serv", " Armed-Forces") # nolint

relationship <- seq(1, 6)
names(relationship) <- c(" Wife", " Own-child", " Husband", " Not-in-family", " Other-relative", " Unmarried") # nolint

race <- seq(1, 5)
names(race) <- c(" White", " Asian-Pac-Islander", " Amer-Indian-Eskimo", " Other", " Black") # nolint

sex <- seq(1, 2)
names(sex) <- c(" Female", " Male")

native_country <- seq(1, 41)
names(native_country) <- c(" United-States", " Cambodia", " England", " Puerto-Rico", " Canada", " Germany", " Outlying-US(Guam-USVI-etc)", " India", " Japan", " Greece", " South", " China", " Cuba", " Iran", " Honduras", " Philippines", " Italy", " Poland", " Jamaica", " Vietnam", " Mexico", " Portugal", " Ireland", " France", " Dominican-Republic", " Laos", " Ecuador", " Taiwan", " Haiti", " Columbia", " Hungary", " Guatemala", " Nicaragua", " Scotland", " Thailand", " Yugoslavia", " El-Salvador", " Trinadad&Tobago", " Peru", " Hong", " Holand-Netherlands") # nolint

income <- seq(1, 2)
names(income) <- c(" >50K", " <=50K")

process_raw_data <- function(data) {
    print("Processing workclass ...")
    workclass_mean <-
        round(mean(categorical_to_numerical(data$workclass, workclass)))
    data$workclass <-
        replace_missing_data(data$workclass, workclass, workclass_mean)

    # Not necessary, identical to Education Num
    # print("Processing education ...")
    # education_mean <-
        # round(mean(categorical_to_numerical(data$education, education)))
    # data$education <-
        # replace_missing_data(data$education, education, education_mean)

    print("Processing marital status ...")
    marital_status_mean <-
        round(mean(categorical_to_numerical(data$marital.status, marital_status)))#nolint
    data$marital_status <-
        replace_missing_data(
            data$marital.status,
            marital_status,
            marital_status_mean
        )
    print("Processing occupation ...")
    occupation_mean <-
        round(mean(categorical_to_numerical(data$occupation, occupation)))
    data$occupation <-
        replace_missing_data(data$occupation, occupation, occupation_mean)

    print("Processing relationship ...")
    relationship_mean <-
        round(mean(categorical_to_numerical(data$relationship, relationship)))
    data$relationship <-
        replace_missing_data(data$relationship, relationship, relationship_mean)

    print("Processing race ...")
    race_mean <-
        round(mean(categorical_to_numerical(data$race, race)))
    data$race <-
        replace_missing_data(data$race, race, race_mean)

    print("Processing sex ...")
    sex_mean <-
        round(mean(categorical_to_numerical(data$sex, sex)))
    data$sex <-
        replace_missing_data(data$sex, sex, sex_mean)

    print("Processing native country ...")
    native_country_mean <-
        round(mean(categorical_to_numerical(data$native.country, native_country)))#nolint
    data$native_country <-
        replace_missing_data(
            data$native.country,
            native_country,
            native_country_mean
        )

    print("Processing income ...")
    income_mean <-
        round(mean(categorical_to_numerical(data$income, income)))
    data$income <-
        replace_missing_data(data$income, income, income_mean)

    print("Renaming vector's names to snake case ...")
    data$education <- data$education.num
    data$capital_gain <- data$capital.gain
    data$capital_loss <- data$capital.loss
    data$hours_per_week <- data$hours.per.week

    print("Cleaning ...")
    data <- subset(data, select = -c(native.country, marital.status, education.num, capital.gain, capital.loss, hours.per.week))#nolint

    print("Done!")
    data
}

standarize_data <- function(data) {
    for (i in seq_len(length(data))) {
       data[i] = scale(data[i], center = T, scale = T)
    }
    data
}
