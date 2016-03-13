SELECT
    INSTNM                    AS "name",
    R.LATITUDE                AS "latitude",
    R.LONGITUDE               AS "longitude",
    CITY                      AS "city",
    STABBR                    AS "stabbr",
    ZIP                       AS "zip",
    YEAR                      AS "year",
    CONTROL                   AS "control",
    region                    AS "region",
    R.CCBASIC                 AS "carnegie",
    R.locale                  AS "locale",
    IFNULL(R.RELAFFIL,'None') AS "religious_affil",
    CASE
        WHEN R.HBCU = 'Yes'
        THEN 1
        ELSE 0
    END AS "historically_black",
    CASE
        WHEN R.PBI = 'Yes'
        THEN 1
        ELSE 0
    END AS "predominantly_black",
    CASE
        WHEN R.ANNHI = 'Yes'
        THEN 1
        ELSE 0
    END AS "alaskan_hawaian",
    CASE
        WHEN R.TRIBAL = 'Yes'
        THEN 1
        ELSE 0
    END AS "tribal",
    CASE
        WHEN R.AANAPII = 'Yes'
        THEN 1
        ELSE 0
    END AS "asian_am_native_am_pacific",
    CASE
        WHEN R.HSI = 'Yes'
        THEN 1
        ELSE 0
    END AS "hispanic",
    CASE
        WHEN R.NANTI = 'Yes'
        THEN 1
        ELSE 0
    END AS "native_american_non_tribal",
    CASE
        WHEN R.MENONLY = 'Yes'
        THEN 1
        ELSE 0
    END AS "men_only",
    CASE
        WHEN R.WOMENONLY = 'Yes'
        THEN 1
        ELSE 0
    END             AS "women_only",
    RET_FT4         AS "retention_rate_four_year_full_time",
    RET_FTL4        AS "retention_rate_lt_four_year_full_time",
    RET_PT4         AS "retention_rate_four_year_part_time",
    RET_PTL4        AS "retention_rate_lt_four_year_part_time",
    ADM_RATE        AS "admission_rate",
    ADM_RATE_ALL    AS "admission_rate_all",
    PREDDEG         AS "predominant_degree",
    HIGHDEG         AS "highest_degree",
    SATVR25         AS "satvr25",
    SATVR75         AS "satvr75",
    SATMT25         AS "satmt25",
    SATMT75         AS "satmt75",
    SATWR25         AS "satwr25",
    SATWR75         AS "satwr75",
    SATVRMID        AS "satvrmid",
    SATMTMID        AS "satmtmid",
    SATWRMID        AS "satwrmid",
    ACTCM25         AS "actcm25",
    ACTCM75         AS "actcm75",
    ACTEN25         AS "acten25",
    ACTEN75         AS "acten75",
    ACTMT25         AS "actmt25",
    ACTMT75         AS "actmt75",
    ACTWR25         AS "actwr25",
    ACTWR75         AS "actwr75",
    ACTCMMID        AS "actcmmid",
    ACTENMID        AS "actenmid",
    ACTMTMID        AS "actmtmid",
    ACTWRMID        AS "actwrmid",
    SAT_AVG         AS "sat_avg",
    SAT_AVG_ALL     AS "sat_avg_all",
    PCIP01          AS "percent_agriculture",
    PCIP03          AS "percent_resources",
    PCIP04          AS "percent_architecture",
    PCIP05          AS "percent_ethnic_cultural_gender",
    PCIP09          AS "percent_communication",
    PCIP10          AS "percent_communications_technology",
    PCIP11          AS "percent_computer",
    PCIP12          AS "percent_personal_culinary",
    PCIP13          AS "percent_education",
    PCIP14          AS "percent_engineering",
    PCIP15          AS "percent_engineering_technology",
    PCIP16          AS "percent_language",
    PCIP19          AS "percent_family_consumer_science",
    PCIP22          AS "percent_legal",
    PCIP23          AS "percent_english",
    PCIP24          AS "percent_humanities",
    PCIP25          AS "percent_library",
    PCIP26          AS "percent_biological",
    PCIP27          AS "percent_mathematics",
    PCIP29          AS "percent_military",
    PCIP30          AS "percent_multidiscipline",
    PCIP31          AS "percent_parks_recreation_fitness",
    PCIP38          AS "percent_philosophy_religious",
    PCIP39          AS "percent_theology_religious_vocation",
    PCIP40          AS "percent_physical_science",
    PCIP41          AS "percent_science_technology",
    PCIP42          AS "percent_psychology",
    PCIP43          AS "percent_security_law_enforcement",
    PCIP44          AS "percent_public_administration_social_service",
    PCIP45          AS "percent_social_science",
    PCIP46          AS "percent_construction",
    PCIP47          AS "percent_mechanic_repair_technology",
    PCIP48          AS "percent_precision_production",
    PCIP49          AS "percent_transportation",
    PCIP50          AS "percent_visual_performing",
    PCIP51          AS "percent_health",
    PCIP52          AS "percent_business_marketing",
    PCIP54          AS "percent_history",
    UGDS_WHITE      AS "race_percent_white",
    UGDS_BLACK      AS "race_percent_black",
    UGDS_HISP       AS "race_percent_hispanic",
    UGDS_ASIAN      AS "race_percent_asian",
    UGDS_AIAN       AS "race_percent_aian",
    UGDS_NHPI       AS "race_percent_nhpi",
    UGDS_2MOR       AS "race_percent_two_or_more",
    UGDS_NRA        AS "race_percent_non_resident_alien",
    UGDS_UNKN       AS "race_percent_unknown",
    UGDS            AS "size",
    TUITIONFEE_IN   AS "tuition_in_state",
    TUITIONFEE_OUT  AS "tuition_out_of_state",
    TUITIONFEE_PROG AS "tuition_program_year",
    TUITFTE         AS 'tuition_revenue_per_fte',
    md_earn_wne_p10 AS "ten_yrs_after_entry_median",
    INEXPFTE        AS "instructional_expenditure_per_fte",
    AVGFACSAL       AS "faculty_salary",
    PFTFAC          AS "ft_faculty_rate",
    COSTT4_A        AS "cost_attendance_academic_year",
    COSTT4_P        AS "cost_attendance_program_year",
    PCTPELL         AS "pell_grant_rate",
    PFTFTUG1_EF     AS "share_first_time_full_time"
FROM
    scorecard
LEFT OUTER JOIN
    (
        SELECT
            unitid,
            opeid ,
            locale ,
            RELAFFIL ,
            HBCU ,
            PBI ,
            ANNHI ,
            TRIBAL ,
            AANAPII ,
            HSI ,
            NANTI ,
            MENONLY ,
            WOMENONLY,
            CCBASIC,
            LATITUDE,
            LONGITUDE
        FROM
            scorecard
        WHERE
            YEAR = 2013) R
ON
    scorecard.opeid = r.opeid
AND scorecard.unitid = r.unitid
WHERE
    YEAR IN (2007,
             2009,
             2011)
AND md_earn_wne_p10 IS NOT NULL