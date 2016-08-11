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
            YEAR(REV_RECGN_DT)*100+MONTH(REV_RECGN_DT)  rev_yr_mth
        FROM
            dbo.IC_SEC_NON_SEC_POSN_F
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
        AND REV_RECGN_DT!='1900-01-01') A
GROUP BY
    rev_yr_mth
ORDER BY
    rev_yr_mth