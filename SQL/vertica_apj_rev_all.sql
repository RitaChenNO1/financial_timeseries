--ALTER TABLE
--    egi.AA_estmt_mth RENAME TO AA_APJ_ESTMT_MTH;
DROP TABLE
    egi.AA_APJ_ESTMT_MTH;
CREATE TABLE
    egi.AA_APJ_ESTMT_MTH AS
SELECT
    ESTMT_REV_RECGN_MTH,
    SEC_NON_SEC_POSN_PRCS_DT,
    SUM(SEC_NON_SEC_POSN_NET_USD_AM) AS rev,
    SUM(CP_SEC_POSN_NET_USD_AM)      AS cp_rev
FROM
    (
        SELECT
            YEAR(ESTMT_REV_RECGN_DT)*100+MONTH(ESTMT_REV_RECGN_DT) ESTMT_REV_RECGN_MTH ,
            SEC_NON_SEC_POSN_PRCS_DT,
            SEC_NON_SEC_POSN_NET_USD_AM,
            CP_SEC_POSN_NET_USD_AM
        FROM
            egi.SRC_IC_SEC_NON_SEC_POSN_APJ_HIST_F)t1
GROUP BY
    ESTMT_REV_RECGN_MTH,
    SEC_NON_SEC_POSN_PRCS_DT;
CREATE TABLE
    egi.AA_APJ_REV_ALL AS
SELECT
    rev_yr_mth,
    SUM(SEC_NON_SEC_POSN_NET_USD_AM) AS SEC_NET_USD_AM,
    SUM(CP_SEC_POSN_NET_USD_AM)      AS CP_SEC_NET_USD_AM
FROM
    (
        SELECT
            REV_RECGN_DT,
            SEC_NON_SEC_POSN_NET_USD_AM,
            CP_SEC_POSN_NET_USD_AM,
            SEC_NON_SEC_POSN_PRCS_DT,
            YEAR(REV_RECGN_DT)*100+MONTH(REV_RECGN_DT) rev_yr_mth
        FROM
            egi.SRC_IC_SEC_NON_SEC_POSN_APJ_HIST_F
        WHERE
            SEC_NON_SEC_POSN_PRCS_DT IN ( '2012-01-31',
                                         '2012-04-30',
                                         '2012-07-31',
                                         '2012-10-31',
                                         '2013-01-31',
                                         '2013-04-30',
                                         '2013-07-31',
                                         '2013-10-31',
                                         '2014-01-31',
                                         '2014-04-30',
                                         '2014-07-31',
                                         '2014-10-31',
                                         '2015-01-31',
                                         '2015-04-30',
                                         '2015-07-31',
                                         '2015-10-31',
                                         '2016-01-31',
                                         '2016-04-30',
                                         '2016-07-31')
        AND REV_RECGN_DT!='1900-01-01') A
GROUP BY
    rev_yr_mth;
CREATE TABLE
    egi.AA_APJ_REV_ALL_PRCSDT AS
SELECT
    rev_yr_mth,
    SEC_NON_SEC_POSN_PRCS_DT,
    SUM(SEC_NON_SEC_POSN_NET_USD_AM) AS SEC_NET_USD_AM,
    SUM(CP_SEC_POSN_NET_USD_AM)      AS CP_SEC_NET_USD_AM
FROM
    (
        SELECT
            REV_RECGN_DT,
            SEC_NON_SEC_POSN_NET_USD_AM,
            CP_SEC_POSN_NET_USD_AM,
            SEC_NON_SEC_POSN_PRCS_DT,
            YEAR(REV_RECGN_DT)*100+MONTH(REV_RECGN_DT) rev_yr_mth
        FROM
            egi.SRC_IC_SEC_NON_SEC_POSN_APJ_HIST_F
        WHERE
            REV_RECGN_DT!='1900-01-01') A
GROUP BY
    rev_yr_mth,
    SEC_NON_SEC_POSN_PRCS_DT
ORDER BY
    rev_yr_mth,
    SEC_NON_SEC_POSN_PRCS_DT;
CREATE TABLE
    egi.AA_APJ_PRODLN_REV_ALL AS
SELECT
    PROD_LN_ID,
    rev_yr_mth,
    SUM(SEC_NON_SEC_POSN_NET_USD_AM) AS SEC_NET_USD_AM,
    SUM(CP_SEC_POSN_NET_USD_AM)      AS CP_SEC_NET_USD_AM
FROM
    (
        SELECT
            REV_RECGN_DT,
            SEC_NON_SEC_POSN_NET_USD_AM,
            CP_SEC_POSN_NET_USD_AM,
            SEC_NON_SEC_POSN_PRCS_DT,
            PROD_LN_ID,
            rev_yr_mth
        FROM
            (
                SELECT
                    REV_RECGN_DT,
                    SEC_NON_SEC_POSN_NET_USD_AM,
                    CP_SEC_POSN_NET_USD_AM,
                    SEC_NON_SEC_POSN_PRCS_DT,
                    PROD_ID,
                    YEAR(REV_RECGN_DT)*100+MONTH(REV_RECGN_DT) rev_yr_mth
                FROM
                    egi.SRC_IC_SEC_NON_SEC_POSN_APJ_HIST_F
                WHERE
                    SEC_NON_SEC_POSN_PRCS_DT IN ( '2012-01-31',
                                                 '2012-04-30',
                                                 '2012-07-31',
                                                 '2012-10-31',
                                                 '2013-01-31',
                                                 '2013-04-30',
                                                 '2013-07-31',
                                                 '2013-10-31',
                                                 '2014-01-31',
                                                 '2014-04-30',
                                                 '2014-07-31',
                                                 '2014-10-31',
                                                 '2015-01-31',
                                                 '2015-04-30',
                                                 '2015-07-31',
                                                 '2015-10-31',
                                                 '2016-01-31',
                                                 '2016-04-30',
                                                 '2016-07-02')
                AND REV_RECGN_DT!='1900-01-01' ) snp
        LEFT JOIN
            EGI.RZ_ENTPRS_PROD_D prod
        ON
            snp.PROD_ID=prod.PROD_ID) A
GROUP BY
    PROD_LN_ID,
    rev_yr_mth
ORDER BY
    PROD_LN_ID,
    rev_yr_mth;
    
    
CREATE TABLE
    egi.AA_APJ_SHIPTO_GEOCD_REV_ALL AS    
SELECT
    GEO_CD,
    rev_yr_mth,
    SUM(SEC_NON_SEC_POSN_NET_USD_AM) AS SEC_NET_USD_AM,
    SUM(CP_SEC_POSN_NET_USD_AM)      AS CP_SEC_NET_USD_AM
FROM
    (
        SELECT
            REV_RECGN_DT,
            SEC_NON_SEC_POSN_NET_USD_AM,
            CP_SEC_POSN_NET_USD_AM,
            SEC_NON_SEC_POSN_PRCS_DT,
            B.GEO_CD,
            rev_yr_mth
        FROM
            (
                SELECT
                    REV_RECGN_DT,
                    SEC_NON_SEC_POSN_NET_USD_AM,
                    CP_SEC_POSN_NET_USD_AM,
                    SEC_NON_SEC_POSN_PRCS_DT,
                    SHPT_CUST_ID,
                    YEAR(REV_RECGN_DT)*100+MONTH(REV_RECGN_DT) rev_yr_mth
                FROM
                    egi.SRC_IC_SEC_NON_SEC_POSN_APJ_HIST_F
                WHERE
                    SEC_NON_SEC_POSN_PRCS_DT IN ( '2012-01-31',
                                                 '2012-04-30',
                                                 '2012-07-31',
                                                 '2012-10-31',
                                                 '2013-01-31',
                                                 '2013-04-30',
                                                 '2013-07-31',
                                                 '2013-10-31',
                                                 '2014-01-31',
                                                 '2014-04-30',
                                                 '2014-07-31',
                                                 '2014-10-31',
                                                 '2015-01-31',
                                                 '2015-04-30',
                                                 '2015-07-31',
                                                 '2015-10-31',
                                                 '2016-01-31',
                                                 '2016-04-30',
                                                 '2016-07-02')
                AND REV_RECGN_DT!='1900-01-01' ) snp
        LEFT JOIN
            egi.RZ_CUST_HIER_D B
        ON
            snp.SHPT_CUST_ID=B.CUST_ID) A
GROUP BY
    GEO_CD,
    rev_yr_mth
ORDER BY
    GEO_CD,
    rev_yr_mth;