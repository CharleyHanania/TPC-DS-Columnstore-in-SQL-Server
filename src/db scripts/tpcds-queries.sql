-- query1
WITH customer_total_return 
     AS (SELECT sr_customer_sk     AS ctr_customer_sk, 
                sr_store_sk        AS ctr_store_sk, 
                Sum(sr_return_amt) AS ctr_total_return 
         FROM   store_returns, 
                date_dim 
         WHERE  sr_returned_date_sk = d_date_sk 
                AND d_year = 2000
         GROUP  BY sr_customer_sk, 
                   sr_store_sk),
high_return AS (
    SELECT ctr_store_sk, Avg(ctr_total_return) * 1.2 AS return_limit
    FROM   customer_total_return ctr2 
    GROUP BY ctr_store_sk
)
SELECT TOP 100 c_customer_id 
FROM   customer_total_return ctr1, 
       store, 
       customer,
       high_return 
WHERE  ctr1.ctr_total_return > high_return.return_limit
       AND s_store_sk = ctr1.ctr_store_sk 
       AND s_state = 'TN' 
       AND ctr1.ctr_customer_sk = c_customer_sk 
       AND ctr1.ctr_store_sk = high_return.ctr_store_sk
ORDER  BY c_customer_id;--

-- query2
WITH wscs 
     AS (SELECT sold_date_sk, 
                sales_price 
         FROM   (SELECT ws_sold_date_sk    sold_date_sk, 
                        ws_ext_sales_price sales_price 
                 FROM   web_sales) q
         UNION ALL 
         (SELECT cs_sold_date_sk    sold_date_sk, 
                 cs_ext_sales_price sales_price 
          FROM   catalog_sales)), 
     wswscs 
     AS (SELECT d_week_seq, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Sunday' ) THEN sales_price 
                      ELSE NULL 
                    END) sun_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Monday' ) THEN sales_price 
                      ELSE NULL 
                    END) mon_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Tuesday' ) THEN sales_price 
                      ELSE NULL 
                    END) tue_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Wednesday' ) THEN sales_price 
                      ELSE NULL 
                    END) wed_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Thursday' ) THEN sales_price 
                      ELSE NULL 
                    END) thu_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Friday' ) THEN sales_price 
                      ELSE NULL 
                    END) fri_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Saturday' ) THEN sales_price 
                      ELSE NULL 
                    END) sat_sales 
         FROM   wscs, 
                date_dim 
         WHERE  d_date_sk = sold_date_sk 
         GROUP  BY d_week_seq) 
SELECT d_week_seq1, 
       Round(sun_sales1 / sun_sales2, 2), 
       Round(mon_sales1 / mon_sales2, 2), 
       Round(tue_sales1 / tue_sales2, 2), 
       Round(wed_sales1 / wed_sales2, 2), 
       Round(thu_sales1 / thu_sales2, 2), 
       Round(fri_sales1 / fri_sales2, 2), 
       Round(sat_sales1 / sat_sales2, 2) 
FROM   (SELECT wswscs.d_week_seq d_week_seq1, 
               sun_sales         sun_sales1, 
               mon_sales         mon_sales1, 
               tue_sales         tue_sales1, 
               wed_sales         wed_sales1, 
               thu_sales         thu_sales1, 
               fri_sales         fri_sales1, 
               sat_sales         sat_sales1 
        FROM   wswscs, 
               date_dim 
        WHERE  date_dim.d_week_seq = wswscs.d_week_seq 
               AND d_year = 2001) y, 
       (SELECT wswscs.d_week_seq d_week_seq2, 
               sun_sales         sun_sales2, 
               mon_sales         mon_sales2, 
               tue_sales         tue_sales2, 
               wed_sales         wed_sales2, 
               thu_sales         thu_sales2, 
               fri_sales         fri_sales2, 
               sat_sales         sat_sales2 
        FROM   wswscs, 
               date_dim 
        WHERE  date_dim.d_week_seq = wswscs.d_week_seq 
               AND d_year = 2001 + 1) z 
WHERE  d_week_seq1 = d_week_seq2 - 53 
ORDER  BY d_week_seq1;--

-- query3
SELECT TOP 100 dt.d_year, 
               item.i_brand_id          brand_id, 
               item.i_brand             brand, 
               Sum(ss_ext_discount_amt) sum_agg 
FROM   date_dim dt, 
       store_sales, 
       item 
WHERE  dt.d_date_sk = store_sales.ss_sold_date_sk 
       AND store_sales.ss_item_sk = item.i_item_sk 
       AND item.i_manufact_id = 128
       AND dt.d_moy = 11 
GROUP  BY dt.d_year, 
          item.i_brand, 
          item.i_brand_id 
ORDER  BY dt.d_year, 
          sum_agg DESC,
          brand_id;--
-- query4
WITH year_total 
     AS (SELECT c_customer_id                       customer_id, 
                d_year                              dyear, 
                Sum(( ( ss_ext_list_price - ss_ext_wholesale_cost 
                        - ss_ext_discount_amt 
                      ) 
                      + 
                          ss_ext_sales_price ) / 2) year_total, 
                's'                                 sale_type 
         FROM   customer, 
                store_sales, 
                date_dim 
         WHERE  c_customer_sk = ss_customer_sk 
                AND ss_sold_date_sk = d_date_sk 
         GROUP  BY c_customer_id, 
                   d_year 
         UNION ALL 
         SELECT c_customer_id                             customer_id, 
                d_year                                    dyear, 
                Sum(( ( ( cs_ext_list_price 
                          - cs_ext_wholesale_cost 
                          - cs_ext_discount_amt 
                        ) + 
                              cs_ext_sales_price ) / 2 )) year_total, 
                'c'                                       sale_type 
         FROM   customer, 
                catalog_sales, 
                date_dim 
         WHERE  c_customer_sk = cs_bill_customer_sk 
                AND cs_sold_date_sk = d_date_sk 
         GROUP  BY c_customer_id, 
                   d_year 
         UNION ALL 
         SELECT c_customer_id                             customer_id, 
                d_year                                    dyear, 
                Sum(( ( ( ws_ext_list_price 
                          - ws_ext_wholesale_cost 
                          - ws_ext_discount_amt 
                        ) + 
                              ws_ext_sales_price ) / 2 )) year_total, 
                'w'                                       sale_type 
         FROM   customer, 
                web_sales, 
                date_dim 
         WHERE  c_customer_sk = ws_bill_customer_sk 
                AND ws_sold_date_sk = d_date_sk 
         GROUP  BY c_customer_id, 
                   d_year) 
SELECT TOP 100 t_s_secyear.customer_id, 
               customer.c_first_name, 
               customer.c_last_name, 
               customer.c_preferred_cust_flag 
FROM   year_total t_s_firstyear, 
       year_total t_s_secyear, 
       year_total t_c_firstyear, 
       year_total t_c_secyear, 
       year_total t_w_firstyear, 
       year_total t_w_secyear,
       customer
WHERE  t_s_secyear.customer_id = t_s_firstyear.customer_id 
       AND t_s_firstyear.customer_id = t_c_secyear.customer_id 
       AND t_s_firstyear.customer_id = t_c_firstyear.customer_id 
       AND t_s_firstyear.customer_id = t_w_firstyear.customer_id 
       AND t_s_firstyear.customer_id = t_w_secyear.customer_id 
       AND t_s_secyear.customer_id = customer.c_customer_id
       AND t_s_firstyear.sale_type = 's' 
       AND t_c_firstyear.sale_type = 'c' 
       AND t_w_firstyear.sale_type = 'w' 
       AND t_s_secyear.sale_type = 's' 
       AND t_c_secyear.sale_type = 'c' 
       AND t_w_secyear.sale_type = 'w' 
       AND t_s_firstyear.dyear = 2001 
       AND t_s_secyear.dyear = 2001 + 1 
       AND t_c_firstyear.dyear = 2001 
       AND t_c_secyear.dyear = 2001 + 1 
       AND t_w_firstyear.dyear = 2001 
       AND t_w_secyear.dyear = 2001 + 1 
       AND t_s_firstyear.year_total > 0 
       AND t_c_firstyear.year_total > 0 
       AND t_w_firstyear.year_total > 0 
       AND CASE 
             WHEN t_c_firstyear.year_total > 0 THEN t_c_secyear.year_total / 
                                                    t_c_firstyear.year_total 
             ELSE NULL 
           END > CASE 
                   WHEN t_s_firstyear.year_total > 0 THEN 
                   t_s_secyear.year_total / 
                   t_s_firstyear.year_total 
                   ELSE NULL 
                 END 
       AND CASE 
             WHEN t_c_firstyear.year_total > 0 THEN t_c_secyear.year_total / 
                                                    t_c_firstyear.year_total 
             ELSE NULL 
           END > CASE 
                   WHEN t_w_firstyear.year_total > 0 THEN 
                   t_w_secyear.year_total / 
                   t_w_firstyear.year_total 
                   ELSE NULL 
                 END 
ORDER  BY t_s_secyear.customer_id;--

-- query5
WITH ssr AS 
( 
         SELECT   s_store_id, 
                  Sum(sales_price) AS sales, 
                  Sum(profit)      AS profit, 
                  Sum(return_amt)  AS returns1, 
                  Sum(net_loss)    AS profit_loss 
         FROM     ( 
                         SELECT ss_store_sk             AS store_sk, 
                                ss_sold_date_sk         AS date_sk, 
                                ss_ext_sales_price      AS sales_price, 
                                ss_net_profit           AS profit, 
                                0 AS return_amt, 
                                0 AS net_loss 
                         FROM   store_sales 
                         UNION ALL 
                         SELECT sr_store_sk             AS store_sk, 
                                sr_returned_date_sk     AS date_sk, 
                                0 AS sales_price, 
                                0 AS profit, 
                                sr_return_amt           AS return_amt, 
                                sr_net_loss             AS net_loss 
                         FROM   store_returns ) salesreturns, 
                  date_dim, 
                  store 
         WHERE    date_sk = d_date_sk 
         AND      Cast(d_date AS DATE) BETWEEN Cast('2000-08-23' AS DATE) AND      ( 
                           Cast('2000-09-06' AS DATE)) 
         AND      store_sk = s_store_sk 
         GROUP BY s_store_id) , csr AS 
( 
         SELECT   cp_catalog_page_id, 
                  sum(sales_price) AS sales, 
                  sum(profit)      AS profit, 
                  sum(return_amt)  AS returns1, 
                  sum(net_loss)    AS profit_loss 
         FROM     ( 
                         SELECT cs_catalog_page_sk      AS page_sk, 
                                cs_sold_date_sk         AS date_sk, 
                                cs_ext_sales_price      AS sales_price, 
                                cs_net_profit           AS profit, 
                                0 AS return_amt, 
                                0 AS net_loss 
                         FROM   catalog_sales 
                         UNION ALL 
                         SELECT cr_catalog_page_sk      AS page_sk, 
                                cr_returned_date_sk     AS date_sk, 
                                0 AS sales_price, 
                                0 AS profit, 
                                cr_return_amount        AS return_amt, 
                                cr_net_loss             AS net_loss 
                         FROM   catalog_returns ) salesreturns, 
                  date_dim, 
                  catalog_page 
         WHERE    date_sk = d_date_sk 
         AND      Cast(d_date AS DATE) BETWEEN Cast('2000-08-23' AS DATE) AND      ( 
                           Cast('2000-09-06' AS DATE)) 
         AND      page_sk = cp_catalog_page_sk 
         GROUP BY cp_catalog_page_id) , wsr AS 
( 
         SELECT   web_site_id, 
                  sum(sales_price) AS sales, 
                  sum(profit)      AS profit, 
                  sum(return_amt)  AS returns1, 
                  sum(net_loss)    AS profit_loss 
         FROM     ( 
                         SELECT ws_web_site_sk          AS wsr_web_site_sk, 
                                ws_sold_date_sk         AS date_sk, 
                                ws_ext_sales_price      AS sales_price, 
                                ws_net_profit           AS profit, 
                                0 AS return_amt, 
                                0 AS net_loss 
                         FROM   web_sales 
                         UNION ALL 
                         SELECT          ws_web_site_sk          AS wsr_web_site_sk, 
                                         wr_returned_date_sk     AS date_sk, 
                                         0 AS sales_price, 
                                         0 AS profit, 
                                         wr_return_amt           AS return_amt, 
                                         wr_net_loss             AS net_loss 
                         FROM            web_returns 
                         LEFT OUTER JOIN web_sales 
                         ON              ( 
                                                         wr_item_sk = ws_item_sk 
                                         AND             wr_order_number = ws_order_number) ) salesreturns,
                  date_dim, 
                  web_site 
         WHERE    date_sk = d_date_sk 
          AND      Cast(d_date AS DATE) BETWEEN Cast('2000-08-23' AS DATE) AND      ( 
                           Cast('2000-09-06' AS DATE))
         AND      wsr_web_site_sk = web_site_sk 
         GROUP BY web_site_id) 
SELECT  TOP 100
         channel , 
         id , 
         sum(sales)   AS sales , 
         sum(returns1) AS returns1 , 
         sum(profit)  AS profit 
FROM     ( 
                SELECT 'store channel' AS channel , 
                       Concat('store', s_store_id) AS id , 
                       sales , 
                       returns1 , 
                       (profit - profit_loss) AS profit 
                FROM   ssr 
                UNION ALL 
                SELECT 'catalog channel' AS channel , 
                       Concat('catalog_page', cp_catalog_page_id) AS id , 
                       sales , 
                       returns1 , 
                       (profit - profit_loss) AS profit 
                FROM   csr 
                UNION ALL 
                SELECT 'web channel' AS channel , 
                       Concat('web_site', web_site_id) AS id , 
                       sales , 
                       returns1 , 
                       (profit - profit_loss) AS profit 
                FROM   wsr ) x 
GROUP BY channel, id
ORDER BY channel , 
         id;-- 

-- query6
SELECT TOP 100 a.ca_state state, 
               Count(*)   cnt 
FROM   customer_address a, 
       customer c, 
       store_sales s, 
       date_dim d, 
       item i 
WHERE  a.ca_address_sk = c.c_current_addr_sk 
       AND c.c_customer_sk = s.ss_customer_sk 
       AND s.ss_sold_date_sk = d.d_date_sk 
       AND s.ss_item_sk = i.i_item_sk 
       AND d.d_month_seq = (SELECT DISTINCT ( d_month_seq ) 
                            FROM   date_dim 
                            WHERE  d_year = 2001
                                   AND d_moy = 1) 
       AND i.i_current_price > 1.2 * (SELECT Avg(j.i_current_price) 
                                      FROM   item j 
                                      WHERE  j.i_category = i.i_category) 
GROUP  BY a.ca_state 
HAVING Count(*) >= 10 
ORDER  BY cnt;--

-- query7
SELECT TOP 100 i_item_id, 
               Avg(ss_quantity)    agg1, 
               Avg(ss_list_price)  agg2, 
               Avg(ss_coupon_amt)  agg3, 
               Avg(ss_sales_price) agg4 
FROM   store_sales, 
       customer_demographics, 
       date_dim, 
       item, 
       promotion 
WHERE  ss_sold_date_sk = d_date_sk 
       AND ss_item_sk = i_item_sk 
       AND ss_cdemo_sk = cd_demo_sk 
       AND ss_promo_sk = p_promo_sk 
       AND cd_gender = 'M' 
       AND cd_marital_status = 'S' 
       AND cd_education_status = 'College' 
       AND ( p_channel_email = 'N' 
              OR p_channel_event = 'N' ) 
       AND d_year = 2000
GROUP  BY i_item_id 
ORDER  BY i_item_id;--

-- query8
select top 100 s_store_name
      ,sum(ss_net_profit)
 from store_sales
     ,date_dim
     ,store,
     (select ca_zip
     from (
      SELECT substring(ca_zip,1,5) ca_zip
      FROM customer_address
      WHERE SUBSTRING(ca_zip,1,5) IN ('65084','87816','83926','77556','20548','26231','43848','15126',
									'91137','61265','98294','25782','17920','18426','98235','40081',
									'84093','28577','55565','17183','54601','67897','22752','86284',
									'18376','38607','45200','21756','29741','96765','23932','89360',
									'72550','10390','18845','47770','82636','41367','76638','86198',
									'81312','37126','39192','88424','72175','81426','53672','10445',
									'42666','66864','66708','41248','48583','82276','18842','78890',
									'49448','14089','38122','77610','13695','34425','28898','91068',
									'79077','19849','43285','39861','66162','99543','83444','83041',
									'12305','57665','68341','25003','57834','62878','49130','81096',
									'18840','27700','23470','50412','21195','16021','76107','71954',
									'68309','18119','98359','64544','10336','86379','27068','39736',
									'98569','28915','24206','56529','57647','54917','42961','91110',
									'63981','14922','36420','23006','67467','32754','30903','20260',
									'31671','51798','72325','85816','68621','13955','36446','41766',
									'68806','16725','15146','22744','35850','88086','51649','18270',
									'52867','39972','96976','63792','11376','94898','13595','10516',
									'90225','58943','39371','94945','28587','96576','57855','28488',
									'26105','83933','25858','34322','13354','45375','40558','56458',
									'44438','73171','30122','34102','22685','71256','78451','54364',
									'28286','45266','47305','69399','83921','26233','11101','15371',
									'69913','35942','15882','25631','24610','44165','99076','33786',
									'70738','26653','14328','72305','62496','22152','10144','64147',
									'48425','14663','21076','18799','30450','63089','81019','68893',
									'24996','51200','51211','45692','92712','70466','79994','22437',
									'25280','38935','71791','73134','56571','14060','19505','72425',
									'56575','74351','68786','51650','20004','18383','76614','11634',
									'18906','15765','41368','73241','76698','78567','97189','28545',
									'76231','75691','22246','51061','90578','56691','68014','51103',
									'94167','57047','14867','73520','15734','63435','25733','35474',
									'24676','94627','53535','17879','15559','53268','59166','11928',
									'59402','33282','45721','43933','68101','33515','36634','71286',
									'19736','58058','55253','67473','41918','19515','36495','19430',
									'22351','77191','91393','49156','50298','87501','18652','53179',
									'18767','63193','23968','65164','68880','21286','72823','58470',
									'67301','13394','31016','70372','67030','40604','24317','45748',
									'39127','26065','77721','31029','31880','60576','24671','45549',
									'13376','50016','33123','19769','22927','97789','46081','72151',
									'15723','46136','51949','68100','96888','64528','14171','79777',
									'28709','11489','25103','32213','78668','22245','15798','27156',
									'37930','62971','21337','51622','67853','10567','38415','15455',
									'58263','42029','60279','37125','56240','88190','50308','26859',
									'64457','89091','82136','62377','36233','63837','58078','17043',
									'30010','60099','28810','98025','29178','87343','73273','30469',
									'64034','39516','86057','21309','90257','67875','40162','11356',
									'73650','61810','72013','30431','22461','19512','13375','55307',
									'30625','83849','68908','26689','96451','38193','46820','88885',
									'84935','69035','83144','47537','56616','94983','48033','69952',
									'25486','61547','27385','61860','58048','56910','16807','17871',
									'35258','31387','35458','35576','29839','25989','24128','76232')
     intersect
      select ca_zip
      from (SELECT SUBSTRING(ca_zip,1,5) ca_zip,count(*) cnt
            FROM customer_address, customer
            WHERE ca_address_sk = c_current_addr_sk and
                  c_preferred_cust_flag='Y'
            group by ca_zip
            having count(*) > 10)A1)A2) V1
 where ss_store_sk = s_store_sk
  and ss_sold_date_sk = d_date_sk
  and d_qoy = 2 and d_year = 1998
  and (SUBSTRING(s_zip,1,2) = SUBSTRING(V1.ca_zip,1,2))
 group by s_store_name
 order by s_store_name;--

-- query9
select case when (select count(*) 
                  from store_sales 
                  where ss_quantity between 1 and 20) > 74129
            then (select avg(ss_ext_discount_amt) 
                  from store_sales 
                  where ss_quantity between 1 and 20) 
            else (select avg(ss_net_paid)
                  from store_sales
                  where ss_quantity between 1 and 20) end bucket1 ,
       case when (select count(*)
                  from store_sales
                  where ss_quantity between 21 and 40) > 122840
            then (select avg(ss_ext_discount_amt)
                  from store_sales
                  where ss_quantity between 21 and 40) 
            else (select avg(ss_net_paid)
                  from store_sales
                  where ss_quantity between 21 and 40) end bucket2,
       case when (select count(*)
                  from store_sales
                  where ss_quantity between 41 and 60) > 56580
            then (select avg(ss_ext_discount_amt)
                  from store_sales
                  where ss_quantity between 41 and 60)
            else (select avg(ss_net_paid)
                  from store_sales
                  where ss_quantity between 41 and 60) end bucket3,
       case when (select count(*)
                  from store_sales
                  where ss_quantity between 61 and 80) > 10097
            then (select avg(ss_ext_discount_amt)
                  from store_sales
                  where ss_quantity between 61 and 80)
            else (select avg(ss_net_paid)
                  from store_sales
                  where ss_quantity between 61 and 80) end bucket4,
       case when (select count(*)
                  from store_sales
                  where ss_quantity between 81 and 100) > 165306
            then (select avg(ss_ext_discount_amt)
                  from store_sales
                  where ss_quantity between 81 and 100)
            else (select avg(ss_net_paid)
                  from store_sales
                  where ss_quantity between 81 and 100) end bucket5
from reason
where r_reason_sk = 1;--

-- query10
SELECT TOP 100 cd_gender, 
               cd_marital_status, 
               cd_education_status, 
               Count(*) cnt1, 
               cd_purchase_estimate, 
               Count(*) cnt2, 
               cd_credit_rating, 
               Count(*) cnt3, 
               cd_dep_count, 
               Count(*) cnt4, 
               cd_dep_employed_count, 
               Count(*) cnt5, 
               cd_dep_college_count, 
               Count(*) cnt6 
FROM   customer c, 
       customer_address ca, 
       customer_demographics 
WHERE  c.c_current_addr_sk = ca.ca_address_sk 
       AND ca_county IN ( 'Rush County', 'Toole County', 
                          'Jefferson County', 
                          'Dona Ana County', 
                           'La Porte County' ) 
       AND cd_demo_sk = c.c_current_cdemo_sk 
       AND EXISTS (SELECT * 
                   FROM   store_sales, 
                          date_dim 
                   WHERE  c.c_customer_sk = ss_customer_sk 
                          AND ss_sold_date_sk = d_date_sk 
                          AND d_year = 2002 
                          AND d_moy BETWEEN 1 AND 4) 
       AND ( EXISTS (SELECT * 
                     FROM   web_sales, 
                            date_dim 
                     WHERE  c.c_customer_sk = ws_bill_customer_sk 
                            AND ws_sold_date_sk = d_date_sk 
                            AND d_year = 2002 
                            AND d_moy BETWEEN 1 AND 1 + 3) 
              OR EXISTS (SELECT * 
                         FROM   catalog_sales, 
                                date_dim 
                         WHERE  c.c_customer_sk = cs_ship_customer_sk 
                                AND cs_sold_date_sk = d_date_sk 
                                AND d_year = 2002 
                                AND d_moy BETWEEN 1 AND 1 + 3) ) 
GROUP  BY cd_gender, 
          cd_marital_status, 
          cd_education_status, 
          cd_purchase_estimate, 
          cd_credit_rating, 
          cd_dep_count, 
          cd_dep_employed_count, 
          cd_dep_college_count 
ORDER  BY cd_gender, 
          cd_marital_status, 
          cd_education_status, 
          cd_purchase_estimate, 
          cd_credit_rating, 
          cd_dep_count, 
          cd_dep_employed_count, 
          cd_dep_college_count;--
          
-- query11
with year_total as (
 select c_customer_id customer_id
       ,c_first_name customer_first_name
       ,c_last_name customer_last_name
       ,c_preferred_cust_flag customer_preferred_cust_flag
       ,c_birth_country customer_birth_country
       ,c_login customer_login
       ,c_email_address customer_email_address
       ,d_year dyear
       ,sum(ss_ext_list_price-ss_ext_discount_amt) year_total
       ,'s' sale_type
 from customer
     ,store_sales
     ,date_dim
 where c_customer_sk = ss_customer_sk
   and ss_sold_date_sk = d_date_sk
 group by c_customer_id
         ,c_first_name
         ,c_last_name
         ,c_preferred_cust_flag 
         ,c_birth_country
         ,c_login
         ,c_email_address
         ,d_year 
 union all
 select c_customer_id customer_id
       ,c_first_name customer_first_name
       ,c_last_name customer_last_name
       ,c_preferred_cust_flag customer_preferred_cust_flag
       ,c_birth_country customer_birth_country
       ,c_login customer_login
       ,c_email_address customer_email_address
       ,d_year dyear
       ,sum(ws_ext_list_price-ws_ext_discount_amt) year_total
       ,'w' sale_type
 from customer
     ,web_sales
     ,date_dim
 where c_customer_sk = ws_bill_customer_sk
   and ws_sold_date_sk = d_date_sk
 group by c_customer_id
         ,c_first_name
         ,c_last_name
         ,c_preferred_cust_flag 
         ,c_birth_country
         ,c_login
         ,c_email_address
         ,d_year
         )
  select top 100 
                  t_s_secyear.customer_id
                 ,t_s_secyear.customer_first_name
                 ,t_s_secyear.customer_last_name
                 ,t_s_secyear.customer_preferred_cust_flag
 from year_total t_s_firstyear
     ,year_total t_s_secyear
     ,year_total t_w_firstyear
     ,year_total t_w_secyear
 where t_s_secyear.customer_id = t_s_firstyear.customer_id
         and t_s_firstyear.customer_id = t_w_secyear.customer_id
         and t_s_firstyear.customer_id = t_w_firstyear.customer_id
         and t_s_firstyear.sale_type = 's'
         and t_w_firstyear.sale_type = 'w'
         and t_s_secyear.sale_type = 's'
         and t_w_secyear.sale_type = 'w'
         and t_s_firstyear.dyear = 2001
         and t_s_secyear.dyear = 2001+1
         and t_w_firstyear.dyear = 2001
         and t_w_secyear.dyear = 2001+1
         and t_s_firstyear.year_total > 0
         and t_w_firstyear.year_total > 0
         and case when t_w_firstyear.year_total > 0 then t_w_secyear.year_total / t_w_firstyear.year_total else 0.0 end
             > case when t_s_firstyear.year_total > 0 then t_s_secyear.year_total / t_s_firstyear.year_total else 0.0 end
 order by t_s_secyear.customer_id
         ,t_s_secyear.customer_first_name
         ,t_s_secyear.customer_last_name
         ,t_s_secyear.customer_email_address;--


-- query12
SELECT TOP 100
         i_item_id , 
         i_item_desc , 
         i_category , 
         i_class , 
         i_current_price , 
         Sum(ws_ext_sales_price)                                                              AS itemrevenue ,
         Sum(ws_ext_sales_price)*100/Sum(Sum(ws_ext_sales_price)) OVER (partition BY i_class) AS revenueratio
FROM     web_sales , 
         item , 
         date_dim 
WHERE    ws_item_sk = i_item_sk 
AND      i_category IN ('Sports', 
                        'Books',
                        'Home') 
AND      ws_sold_date_sk = d_date_sk 
AND      Cast(d_date AS DATE) BETWEEN Cast('1999-02-22' AS DATE) AND      ( 
                  Cast('1999-03-24' AS DATE)) 
GROUP BY i_item_id , 
         i_item_desc , 
         i_category , 
         i_class , 
         i_current_price 
ORDER BY i_category , 
         i_class , 
         i_item_id , 
         i_item_desc , 
         revenueratio;-- 

-- query13
SELECT Avg(ss_quantity), 
       Avg(ss_ext_sales_price), 
       Avg(ss_ext_wholesale_cost), 
       Sum(ss_ext_wholesale_cost) 
FROM   store_sales, 
       store, 
       customer_demographics, 
       household_demographics, 
       customer_address, 
       date_dim 
WHERE  s_store_sk = ss_store_sk 
       AND ss_sold_date_sk = d_date_sk 
       AND d_year = 2001 
       AND ( ( ss_hdemo_sk = hd_demo_sk 
               AND cd_demo_sk = ss_cdemo_sk 
               AND cd_marital_status = 'M' 
               AND cd_education_status = 'Advanced Degree' 
               AND ss_sales_price BETWEEN 100.00 AND 150.00 
               AND hd_dep_count = 3 ) 
              OR ( ss_hdemo_sk = hd_demo_sk 
                   AND cd_demo_sk = ss_cdemo_sk 
                   AND cd_marital_status = 'S' 
                   AND cd_education_status = 'College' 
                   AND ss_sales_price BETWEEN 50.00 AND 100.00 
                   AND hd_dep_count = 1 ) 
              OR ( ss_hdemo_sk = hd_demo_sk 
                   AND cd_demo_sk = ss_cdemo_sk 
                   AND cd_marital_status = 'W' 
                   AND cd_education_status = '2 yr Degree' 
                   AND ss_sales_price BETWEEN 150.00 AND 200.00 
                   AND hd_dep_count = 1 ) ) 
       AND ( ( ss_addr_sk = ca_address_sk 
               AND ca_country = 'United States' 
               AND ca_state IN ( 'TX', 'OH', 'TX' ) 
               AND ss_net_profit BETWEEN 100 AND 200 ) 
              OR ( ss_addr_sk = ca_address_sk 
                   AND ca_country = 'United States' 
                   AND ca_state IN ( 'OR', 'NM', 'KY' ) 
                   AND ss_net_profit BETWEEN 150 AND 300 ) 
              OR ( ss_addr_sk = ca_address_sk 
                   AND ca_country = 'United States' 
                   AND ca_state IN ( 'VA', 'TX', 'MS' ) 
                   AND ss_net_profit BETWEEN 50 AND 250 ) );-- 

-- query14
WITH item_ss AS (
    SELECT DISTINCT 
        iss.i_brand_id, 
        iss.i_class_id, 
        iss.i_category_id 
    FROM   store_sales, 
        item iss, 
        date_dim d1 
    WHERE  ss_item_sk = iss.i_item_sk 
        AND ss_sold_date_sk = d1.d_date_sk 
        AND d1.d_year BETWEEN 1999 AND 1999 + 2 
), item_cs AS (
    SELECT DISTINCT 
        ics.i_brand_id, 
        ics.i_class_id, 
        ics.i_category_id 
    FROM   catalog_sales, 
        item ics, 
        date_dim d2 
    WHERE  cs_item_sk = ics.i_item_sk 
        AND cs_sold_date_sk = d2.d_date_sk 
        AND d2.d_year BETWEEN 1999 AND 1999 + 2 
), item_ws AS (
    SELECT DISTINCT 
        iws.i_brand_id, 
        iws.i_class_id, 
        iws.i_category_id 
    FROM   web_sales, 
        item iws, 
        date_dim d3 
    WHERE  ws_item_sk = iws.i_item_sk 
        AND ws_sold_date_sk = d3.d_date_sk 
        AND d3.d_year BETWEEN 1999 AND 1999 + 2
), item_intersect AS (
    SELECT
        item_ss.i_brand_id    brand_id, 
        item_ss.i_class_id    class_id, 
        item_ss.i_category_id category_id 
    FROM item_ss 
    JOIN item_ws ON item_ss.i_brand_id = item_ws.i_brand_id 
        AND item_ss.i_class_id = item_ws.i_class_id 
        AND item_ss.i_category_id = item_ws.i_category_id 
    JOIN item_cs ON item_ss.i_brand_id = item_cs.i_brand_id 
        AND item_ss.i_class_id = item_cs.i_class_id 
        AND item_ss.i_category_id = item_cs.i_category_id 
), cross_items AS (
         SELECT i_item_sk ss_item_sk 
         FROM   item, 
                item_intersect
         WHERE  i_brand_id = brand_id 
                AND i_class_id = class_id 
                AND i_category_id = category_id), 
     avg_sales 
     AS (SELECT Avg(quantity * list_price) average_sales 
         FROM   (SELECT ss_quantity   quantity, 
                        ss_list_price list_price 
                 FROM   store_sales, 
                        date_dim 
                 WHERE  ss_sold_date_sk = d_date_sk 
                        AND d_year BETWEEN 1999 AND 1999 + 2 
                 UNION ALL 
                 SELECT cs_quantity   quantity, 
                        cs_list_price list_price 
                 FROM   catalog_sales, 
                        date_dim 
                 WHERE  cs_sold_date_sk = d_date_sk 
                        AND d_year BETWEEN 1999 AND 1999 + 2 
                 UNION ALL 
                 SELECT ws_quantity   quantity, 
                        ws_list_price list_price 
                 FROM   web_sales, 
                        date_dim 
                 WHERE  ws_sold_date_sk = d_date_sk 
                        AND d_year BETWEEN 1999 AND 1999 + 2) x) 
SELECT TOP 100 channel, 
               i_brand_id, 
               i_class_id, 
               i_category_id, 
               Sum(sales), 
               Sum(number_sales) 
FROM  (SELECT 'store'                          channel, 
              i_brand_id, 
              i_class_id, 
              i_category_id, 
              Sum(ss_quantity * ss_list_price) sales, 
              Count(*)                         number_sales 
       FROM   store_sales, 
              item, 
              date_dim 
       WHERE  ss_item_sk IN (SELECT ss_item_sk 
                             FROM   cross_items) 
              AND ss_item_sk = i_item_sk 
              AND ss_sold_date_sk = d_date_sk 
              AND d_year = 1999 + 2 
              AND d_moy = 11 
       GROUP  BY i_brand_id, 
                 i_class_id, 
                 i_category_id 
       HAVING Sum(ss_quantity * ss_list_price) > (SELECT average_sales 
                                                  FROM   avg_sales) 
       UNION ALL 
       SELECT 'catalog'                        channel, 
              i_brand_id, 
              i_class_id, 
              i_category_id, 
              Sum(cs_quantity * cs_list_price) sales, 
              Count(*)                         number_sales 
       FROM   catalog_sales, 
              item, 
              date_dim 
       WHERE  cs_item_sk IN (SELECT ss_item_sk 
                             FROM   cross_items) 
              AND cs_item_sk = i_item_sk 
              AND cs_sold_date_sk = d_date_sk 
              AND d_year = 1999 + 2 
              AND d_moy = 11 
       GROUP  BY i_brand_id, 
                 i_class_id, 
                 i_category_id 
       HAVING Sum(cs_quantity * cs_list_price) > (SELECT average_sales 
                                                  FROM   avg_sales) 
       UNION ALL 
       SELECT 'web'                            channel, 
              i_brand_id, 
              i_class_id, 
              i_category_id, 
              Sum(ws_quantity * ws_list_price) sales, 
              Count(*)                         number_sales 
       FROM   web_sales, 
              item, 
              date_dim 
       WHERE  ws_item_sk IN (SELECT ss_item_sk 
                             FROM   cross_items) 
              AND ws_item_sk = i_item_sk 
              AND ws_sold_date_sk = d_date_sk 
              AND d_year = 1999 + 2 
              AND d_moy = 11 
       GROUP  BY i_brand_id, 
                 i_class_id, 
                 i_category_id 
       HAVING Sum(ws_quantity * ws_list_price) > (SELECT average_sales 
                                                  FROM   avg_sales)) y 
GROUP  BY channel, i_brand_id, i_class_id, i_category_id 
ORDER  BY channel, 
          i_brand_id, 
          i_class_id, 
          i_category_id;--

-- query15
SELECT TOP 100 ca_zip, 
               Sum(cs_sales_price) 
FROM   catalog_sales, 
       customer, 
       customer_address, 
       date_dim 
WHERE  cs_bill_customer_sk = c_customer_sk 
       AND c_current_addr_sk = ca_address_sk 
       AND ( Substring(ca_zip, 1, 5) IN ( '85669', '86197', '88274', '83405', 
                                       '86475', '85392', '85460', '80348', 
                                       '81792' ) 
              OR ca_state IN ( 'CA', 'WA', 'GA' ) 
              OR cs_sales_price > 500 ) 
       AND cs_sold_date_sk = d_date_sk 
       AND d_qoy = 2 
       AND d_year = 2001 
GROUP  BY ca_zip 
ORDER  BY ca_zip;--

-- query16
SELECT TOP 100
         Count(DISTINCT cs_order_number) AS order_count ,
         Sum(cs_ext_ship_cost)           AS total_shipping_cost ,
         Sum(cs_net_profit)              AS total_net_profit
FROM     catalog_sales cs1 ,
         date_dim ,
         customer_address ,
         call_center
WHERE    Cast(d_date AS DATE) BETWEEN Cast('2002-2-01' AS DATE) AND (
                  Cast('2002-4-02' AS DATE))
AND      cs1.cs_ship_date_sk = d_date_sk
AND      cs1.cs_ship_addr_sk = ca_address_sk
AND      ca_state = 'GA'
AND      cs1.cs_call_center_sk = cc_call_center_sk
AND      cc_county IN ('Williamson County',
                       'Williamson County',
                       'Williamson County',
                       'Williamson County',
                       'Williamson County' )
AND      EXISTS
         (
                SELECT *
                FROM   catalog_sales cs2
                WHERE  cs1.cs_order_number = cs2.cs_order_number
                AND    cs1.cs_warehouse_sk <> cs2.cs_warehouse_sk)
AND      NOT EXISTS
         (
                SELECT *
                FROM   catalog_returns cr1
                WHERE  cs1.cs_order_number = cr1.cr_order_number)
ORDER BY count(DISTINCT cs_order_number);--

-- query17
SELECT TOP 100 i_item_id, 
               i_item_desc, 
               s_state, 
               Count(ss_quantity)                                        AS 
               store_sales_quantitycount, 
               Avg(ss_quantity)                                          AS 
               store_sales_quantityave, 
               Stdev(ss_quantity)                                  AS 
               store_sales_quantitystdev, 
               Stdev(ss_quantity) / Avg(ss_quantity)               AS 
               store_sales_quantitycov, 
               Count(sr_return_quantity)                                 AS 
               store_returns_quantitycount, 
               Avg(sr_return_quantity)                                   AS 
               store_returns_quantityave, 
               Stdev(sr_return_quantity)                           AS 
               store_returns_quantitystdev, 
               Stdev(sr_return_quantity) / Avg(sr_return_quantity) AS 
               store_returns_quantitycov, 
               Count(cs_quantity)                                        AS 
               catalog_sales_quantitycount, 
               Avg(cs_quantity)                                          AS 
               catalog_sales_quantityave, 
               Stdev(cs_quantity) / Avg(cs_quantity)               AS 
               catalog_sales_quantitystdev, 
               Stdev(cs_quantity) / Avg(cs_quantity)               AS 
               catalog_sales_quantitycov 
FROM   store_sales, 
       store_returns, 
       catalog_sales, 
       date_dim d1, 
       date_dim d2, 
       date_dim d3, 
       store, 
       item 
WHERE  d1.d_quarter_name = '2001Q1' 
       AND d1.d_date_sk = ss_sold_date_sk 
       AND i_item_sk = ss_item_sk 
       AND s_store_sk = ss_store_sk 
       AND ss_customer_sk = sr_customer_sk 
       AND ss_item_sk = sr_item_sk 
       AND ss_ticket_number = sr_ticket_number 
       AND sr_returned_date_sk = d2.d_date_sk 
       AND d2.d_quarter_name IN ( '2001Q1', '2001Q2', '2001Q3' ) 
       AND sr_customer_sk = cs_bill_customer_sk 
       AND sr_item_sk = cs_item_sk 
       AND cs_sold_date_sk = d3.d_date_sk 
       AND d3.d_quarter_name IN ( '2001Q1', '2001Q2', '2001Q3' ) 
GROUP  BY i_item_id, 
          i_item_desc, 
          s_state 
ORDER  BY i_item_id, 
          i_item_desc, 
          s_state;--

-- query18
select top 100 i_item_id,
        ca_country,
        ca_state, 
        ca_county,
        avg( cast(cs_quantity as decimal(12,2))) agg1,
        avg( cast(cs_list_price as decimal(12,2))) agg2,
        avg( cast(cs_coupon_amt as decimal(12,2))) agg3,
        avg( cast(cs_sales_price as decimal(12,2))) agg4,
        avg( cast(cs_net_profit as decimal(12,2))) agg5,
        avg( cast(c_birth_year as decimal(12,2))) agg6,
        avg( cast(cd1.cd_dep_count as decimal(12,2))) agg7
 from catalog_sales, customer_demographics cd1, 
      customer_demographics cd2, customer, customer_address, date_dim, item
 where cs_sold_date_sk = d_date_sk and
       cs_item_sk = i_item_sk and
       cs_bill_cdemo_sk = cd1.cd_demo_sk and
       cs_bill_customer_sk = c_customer_sk and
       cd1.cd_gender = 'F' and 
       cd1.cd_education_status = 'Unknown' and
       c_current_cdemo_sk = cd2.cd_demo_sk and
       c_current_addr_sk = ca_address_sk and
       c_birth_month in (1,6,8,9,12,2) and
       d_year = 1998 and
       ca_state in ('MS','IN','ND'
                   ,'OK','NM','VA','MS')
 group by rollup (i_item_id, ca_country, ca_state, ca_county)
 order by ca_country,
        ca_state, 
        ca_county,
	i_item_id;--

-- query19
SELECT TOP 100
               i_brand_id              brand_id, 
               i_brand                 brand, 
               i_manufact_id, 
               i_manufact, 
               Sum(ss_ext_sales_price) ext_price 
FROM   date_dim, 
       store_sales, 
       item, 
       customer, 
       customer_address, 
       store 
WHERE  d_date_sk = ss_sold_date_sk 
       AND ss_item_sk = i_item_sk 
       AND i_manager_id = 8
       AND d_moy = 11
       AND d_year = 1998 
       AND ss_customer_sk = c_customer_sk 
       AND c_current_addr_sk = ca_address_sk 
       AND Substring(ca_zip, 1, 5) <> Substring(s_zip, 1, 5) 
       AND ss_store_sk = s_store_sk 
GROUP  BY i_brand, 
          i_brand_id, 
          i_manufact_id, 
          i_manufact 
ORDER  BY ext_price DESC, 
          i_brand, 
          i_brand_id, 
          i_manufact_id, 
          i_manufact;--

-- query20
SELECT TOP 100
         i_item_id , 
         i_item_desc , 
         i_category , 
         i_class , 
         i_current_price , 
         Sum(cs_ext_sales_price)                                                              AS itemrevenue ,
         Sum(cs_ext_sales_price)*100/Sum(Sum(cs_ext_sales_price)) OVER (partition BY i_class) AS revenueratio
FROM     catalog_sales , 
         item , 
         date_dim 
WHERE    cs_item_sk = i_item_sk 
AND      i_category IN ('Sports', 
                        'Books', 
                        'Home') 
AND      cs_sold_date_sk = d_date_sk 
AND      Cast(d_date AS DATE) BETWEEN Cast('1999-02-22' AS DATE) AND      ( 
                  Cast('1999-03-24' AS DATE)) 
GROUP BY i_item_id , 
         i_item_desc , 
         i_category , 
         i_class , 
         i_current_price 
ORDER BY i_category , 
         i_class , 
         i_item_id , 
         i_item_desc , 
         revenueratio;-- 

-- query21
select top 100 *
 from(select w_warehouse_name
            ,i_item_id
            ,sum(case when (cast(d_date as date) < cast ('2000-03-11' as date))
	                then inv_quantity_on_hand 
                      else 0 end) as inv_before
            ,sum(case when (cast(d_date as date) >= cast ('2000-03-11' as date))
                      then inv_quantity_on_hand 
                      else 0 end) as inv_after
   from inventory
       ,warehouse
       ,item
       ,date_dim
   where i_current_price between 0.99 and 1.49
     and i_item_sk          = inv_item_sk
     and inv_warehouse_sk   = w_warehouse_sk
     and inv_date_sk    = d_date_sk
     and d_date between (dateadd(day,-30,cast ('2000-03-11' as date)))
                    and (dateadd(day,30,cast ('2000-03-11' as date)))
   group by w_warehouse_name, i_item_id) x
 where (case when inv_before > 0 
             then inv_after / inv_before 
             else null
             end) between 2.0/3.0 and 3.0/2.0
 order by w_warehouse_name,i_item_id;--

-- query22

select top 100 i_product_name
             ,i_brand
             ,i_class
             ,i_category
             ,avg(cast(inv_quantity_on_hand as bigint)) qoh
       from inventory
           ,date_dim
           ,item
       where inv_date_sk=d_date_sk
              and inv_item_sk=i_item_sk
              and d_month_seq between 1200 and 1200 + 11
       group by rollup(i_product_name
                       ,i_brand
                       ,i_class
                       ,i_category)
order by qoh, i_product_name, i_brand, i_class, i_category;--

-- query23
with frequent_ss_items as 
 (select SUBSTRING(i_item_desc,1,30) itemdesc,i_item_sk item_sk,d_date solddate,count(*) cnt
  from store_sales
      ,date_dim 
      ,item
  where ss_sold_date_sk = d_date_sk
    and ss_item_sk = i_item_sk 
    and d_year in (2000,2000+1,2000+2,2000+3)
  group by SUBSTRING(i_item_desc,1,30),i_item_sk,d_date
  having count(*) >4),
 max_store_sales as
 (select max(csales) tpcds_cmax 
  from (select c_customer_sk,sum(ss_quantity*ss_sales_price) csales
        from store_sales
            ,customer
            ,date_dim 
        where ss_customer_sk = c_customer_sk
         and ss_sold_date_sk = d_date_sk
         and d_year in (2000,2000+1,2000+2,2000+3) 
        group by c_customer_sk) a),
 best_ss_customer as
 (select c_customer_sk,sum(ss_quantity*ss_sales_price) ssales
  from store_sales
      ,customer
  where ss_customer_sk = c_customer_sk
  group by c_customer_sk
  having sum(ss_quantity*ss_sales_price) > (50/100.0) * (select
  *
from
 max_store_sales))
  select top 100 sum(sales)
 from (select cs_quantity*cs_list_price sales
       from catalog_sales
           ,date_dim 
       where d_year = 2000 
         and d_moy = 2 
         and cs_sold_date_sk = d_date_sk 
         and cs_item_sk in (select item_sk from frequent_ss_items)
         and cs_bill_customer_sk in (select c_customer_sk from best_ss_customer)
      union all
      select ws_quantity*ws_list_price sales
       from web_sales 
           ,date_dim 
       where d_year = 2000 
         and d_moy = 2
         and ws_sold_date_sk = d_date_sk 
         and ws_item_sk in (select item_sk from frequent_ss_items)
         and ws_bill_customer_sk in (select c_customer_sk from best_ss_customer)) a;
with frequent_ss_items as
 (select SUBSTRING(i_item_desc,1,30) itemdesc,i_item_sk item_sk,d_date solddate,count(*) cnt
  from store_sales
      ,date_dim
      ,item
  where ss_sold_date_sk = d_date_sk
    and ss_item_sk = i_item_sk
    and d_year in (2000,2000 + 1,2000 + 2,2000 + 3)
  group by SUBSTRING(i_item_desc,1,30),i_item_sk,d_date
  having count(*) >4),
 max_store_sales as
 (select max(csales) tpcds_cmax
  from (select c_customer_sk,sum(ss_quantity*ss_sales_price) csales
        from store_sales
            ,customer
            ,date_dim 
        where ss_customer_sk = c_customer_sk
         and ss_sold_date_sk = d_date_sk
         and d_year in (2000,2000+1,2000+2,2000+3)
        group by c_customer_sk) a),
 best_ss_customer as
 (select c_customer_sk,sum(ss_quantity*ss_sales_price) ssales
  from store_sales
      ,customer
  where ss_customer_sk = c_customer_sk
  group by c_customer_sk
  having sum(ss_quantity*ss_sales_price) > (50/100.0) * (select
  *
 from max_store_sales))
  select top 100 c_last_name,c_first_name,sales
 from (select c_last_name,c_first_name,sum(cs_quantity*cs_list_price) sales
        from catalog_sales
            ,customer
            ,date_dim 
        where d_year = 2000 
         and d_moy = 2 
         and cs_sold_date_sk = d_date_sk 
         and cs_item_sk in (select item_sk from frequent_ss_items)
         and cs_bill_customer_sk in (select c_customer_sk from best_ss_customer)
         and cs_bill_customer_sk = c_customer_sk 
       group by c_last_name,c_first_name
      union all
      select c_last_name,c_first_name,sum(ws_quantity*ws_list_price) sales
       from web_sales
           ,customer
           ,date_dim 
       where d_year = 2000 
         and d_moy = 2 
         and ws_sold_date_sk = d_date_sk 
         and ws_item_sk in (select item_sk from frequent_ss_items)
         and ws_bill_customer_sk in (select c_customer_sk from best_ss_customer)
         and ws_bill_customer_sk = c_customer_sk
       group by c_last_name,c_first_name) a
     order by c_last_name,c_first_name,sales;--


-- query24
with ssales as
(select c_last_name
      ,c_first_name
      ,s_store_name
      ,ca_state
      ,s_state
      ,i_color
      ,i_current_price
      ,i_manager_id
      ,i_units
      ,i_size
      ,sum(ss_net_paid) netpaid
from store_sales
    ,store_returns
    ,store
    ,item
    ,customer
    ,customer_address
where ss_ticket_number = sr_ticket_number
  and ss_item_sk = sr_item_sk
  and ss_customer_sk = c_customer_sk
  and ss_item_sk = i_item_sk
  and ss_store_sk = s_store_sk
  and c_current_addr_sk = ca_address_sk
  and c_birth_country <> upper(ca_country)
  and s_zip = ca_zip
and s_market_id=8
group by c_last_name
        ,c_first_name
        ,s_store_name
        ,ca_state
        ,s_state
        ,i_color
        ,i_current_price
        ,i_manager_id
        ,i_units
        ,i_size)
select c_last_name
      ,c_first_name
      ,s_store_name
      ,sum(netpaid) paid
from ssales
where i_color = 'saddle'
group by c_last_name
        ,c_first_name
        ,s_store_name
having sum(netpaid) > (select 0.05*avg(netpaid)
                                 from ssales)
order by c_last_name
        ,c_first_name
        ,s_store_name;--

-- query25
SELECT TOP 100 i_item_id, 
               i_item_desc, 
               s_store_id, 
               s_store_name, 
               SUM(ss_net_profit) AS store_sales_profit, 
               SUM(sr_net_loss)   AS store_returns_loss, 
               sum(cs_net_profit) AS catalog_sales_profit 
FROM   store_sales, 
       store_returns, 
       catalog_sales, 
       date_dim d1, 
       date_dim d2, 
       date_dim d3, 
       store, 
       item 
WHERE  d1.d_moy = 4 
       AND d1.d_year = 2001 
       AND d1.d_date_sk = ss_sold_date_sk 
       AND i_item_sk = ss_item_sk 
       AND s_store_sk = ss_store_sk 
       AND ss_customer_sk = sr_customer_sk 
       AND ss_item_sk = sr_item_sk 
       AND ss_ticket_number = sr_ticket_number 
       AND sr_returned_date_sk = d2.d_date_sk 
       AND d2.d_moy BETWEEN 4 AND 10 
       AND d2.d_year = 2001 
       AND sr_customer_sk = cs_bill_customer_sk 
       AND sr_item_sk = cs_item_sk 
       AND cs_sold_date_sk = d3.d_date_sk 
       AND d3.d_moy BETWEEN 4 AND 10 
       AND d3.d_year = 2001 
GROUP  BY i_item_id, 
          i_item_desc, 
          s_store_id, 
          s_store_name 
ORDER  BY i_item_id, 
          i_item_desc, 
          s_store_id, 
          s_store_name;--

-- query26
SELECT TOP 100 i_item_id, 
               Avg(cs_quantity)    agg1, 
               Avg(cs_list_price)  agg2, 
               Avg(cs_coupon_amt)  agg3, 
               Avg(cs_sales_price) agg4 
FROM   catalog_sales, 
       customer_demographics, 
       date_dim, 
       item, 
       promotion 
WHERE  cs_sold_date_sk = d_date_sk 
       AND cs_item_sk = i_item_sk 
       AND cs_bill_cdemo_sk = cd_demo_sk 
       AND cs_promo_sk = p_promo_sk 
       AND cd_gender = 'M' 
       AND cd_marital_status = 'S' 
       AND cd_education_status = 'College' 
       AND ( p_channel_email = 'N' 
              OR p_channel_event = 'N' ) 
       AND d_year = 2000 
GROUP  BY i_item_id 
ORDER  BY i_item_id;--

-- query27
select top 100 i_item_id,
        s_state, grouping(s_state) g_state,
        avg(ss_quantity) agg1,
        avg(ss_list_price) agg2,
        avg(ss_coupon_amt) agg3,
        avg(ss_sales_price) agg4
 from store_sales, customer_demographics, date_dim, store, item
 where ss_sold_date_sk = d_date_sk and
       ss_item_sk = i_item_sk and
       ss_store_sk = s_store_sk and
       ss_cdemo_sk = cd_demo_sk and
       cd_gender = 'M' and
       cd_marital_status = 'S' and
       cd_education_status = 'College' and
       d_year = 1999 and
       s_state in ('TN','TN', 'TN', 'TN', 'TN', 'TN')
 group by rollup (i_item_id, s_state)
 order by i_item_id
         ,s_state;--

-- query28
select top 100 *
from (select avg(ss_list_price) B1_LP
            ,count(ss_list_price) B1_CNT
            ,count(distinct ss_list_price) B1_CNTD
      from store_sales
      where ss_quantity between 0 and 5
        and (ss_list_price between 8 and 8+10 
             or ss_coupon_amt between 459 and 459+1000
             or ss_wholesale_cost between 57 and 57+20)) B1,
     (select avg(ss_list_price) B2_LP
            ,count(ss_list_price) B2_CNT
            ,count(distinct ss_list_price) B2_CNTD
      from store_sales
      where ss_quantity between 6 and 10
        and (ss_list_price between 90 and 90+10
          or ss_coupon_amt between 2323 and 2323+1000
          or ss_wholesale_cost between 31 and 31+20)) B2,
     (select avg(ss_list_price) B3_LP
            ,count(ss_list_price) B3_CNT
            ,count(distinct ss_list_price) B3_CNTD
      from store_sales
      where ss_quantity between 11 and 15
        and (ss_list_price between 142 and 142+10
          or ss_coupon_amt between 12214 and 12214+1000
          or ss_wholesale_cost between 79 and 79+20)) B3,
     (select avg(ss_list_price) B4_LP
            ,count(ss_list_price) B4_CNT
            ,count(distinct ss_list_price) B4_CNTD
      from store_sales
      where ss_quantity between 16 and 20
        and (ss_list_price between 135 and 135+10
          or ss_coupon_amt between 6071 and 6071+1000
          or ss_wholesale_cost between 38 and 38+20)) B4,
     (select avg(ss_list_price) B5_LP
            ,count(ss_list_price) B5_CNT
            ,count(distinct ss_list_price) B5_CNTD
      from store_sales
      where ss_quantity between 21 and 25
        and (ss_list_price between 122 and 122+10
          or ss_coupon_amt between 836 and 836+1000
          or ss_wholesale_cost between 17 and 17+20)) B5,
     (select avg(ss_list_price) B6_LP
            ,count(ss_list_price) B6_CNT
            ,count(distinct ss_list_price) B6_CNTD
      from store_sales
      where ss_quantity between 26 and 30
        and (ss_list_price between 154 and 154+10
          or ss_coupon_amt between 7326 and 7326+1000
          or ss_wholesale_cost between 7 and 7+20)) B6;--


-- query29

select top 100  
     i_item_id
    ,i_item_desc
    ,s_store_id
    ,s_store_name
    ,SUM(ss_quantity)        as store_sales_quantity
    ,sum(sr_return_quantity) as store_returns_quantity
    ,sum(cs_quantity)        as catalog_sales_quantity
 from
    store_sales
   ,store_returns
   ,catalog_sales
   ,date_dim             d1
   ,date_dim             d2
   ,date_dim             d3
   ,store
   ,item
 where
     d1.d_moy               = 9 
 and d1.d_year              = 1999
 and d1.d_date_sk           = ss_sold_date_sk
 and i_item_sk              = ss_item_sk
 and s_store_sk             = ss_store_sk
 and ss_customer_sk         = sr_customer_sk
 and ss_item_sk             = sr_item_sk
 and ss_ticket_number       = sr_ticket_number
 and sr_returned_date_sk    = d2.d_date_sk
 and d2.d_moy               between 9 and  9 + 3 
 and d2.d_year              = 1999
 and sr_customer_sk         = cs_bill_customer_sk
 and sr_item_sk             = cs_item_sk
 and cs_sold_date_sk        = d3.d_date_sk     
 and d3.d_year              in (1999,1999+1,1999+2)
 group by
    i_item_id
   ,i_item_desc
   ,s_store_id
   ,s_store_name
 order by
    i_item_id 
   ,i_item_desc
   ,s_store_id
   ,s_store_name ;--

-- query30
with customer_total_return as
 (select wr_returning_customer_sk as ctr_customer_sk
        ,ca_state as ctr_state, 
 	sum(wr_return_amt) as ctr_total_return
 from web_returns
     ,date_dim
     ,customer_address
 where wr_returned_date_sk = d_date_sk 
   and d_year =2002
   and wr_returning_addr_sk = ca_address_sk 
 group by wr_returning_customer_sk
         ,ca_state)
  select top 100 c_customer_id,c_salutation,c_first_name,c_last_name,c_preferred_cust_flag
       ,c_birth_day,c_birth_month,c_birth_year,c_birth_country,c_login,c_email_address
       ,c_last_review_date,ctr_total_return
 from customer_total_return ctr1
     ,customer_address
     ,customer
 where ctr1.ctr_total_return > (select avg(ctr_total_return)*1.2
 			  from customer_total_return ctr2 
                  	  where ctr1.ctr_state = ctr2.ctr_state)
       and ca_address_sk = c_current_addr_sk
       and ca_state = 'GA'
       and ctr1.ctr_customer_sk = c_customer_sk
 order by c_customer_id,c_salutation,c_first_name,c_last_name,c_preferred_cust_flag
                  ,c_birth_day,c_birth_month,c_birth_year,c_birth_country,c_login,c_email_address
                  ,c_last_review_date,ctr_total_return;--

-- query31
with ss as
 (select ca_county,d_qoy, d_year,sum(ss_ext_sales_price) as store_sales
 from store_sales,date_dim,customer_address
 where ss_sold_date_sk = d_date_sk
  and ss_addr_sk=ca_address_sk
 group by ca_county,d_qoy, d_year),
 ws as
 (select ca_county,d_qoy, d_year,sum(ws_ext_sales_price) as web_sales
 from web_sales,date_dim,customer_address
 where ws_sold_date_sk = d_date_sk
  and ws_bill_addr_sk=ca_address_sk
 group by ca_county,d_qoy, d_year)
 select 
        ss1.ca_county
       ,ss1.d_year
       ,ws2.web_sales/ws1.web_sales web_q1_q2_increase
       ,ss2.store_sales/ss1.store_sales store_q1_q2_increase
       ,ws3.web_sales/ws2.web_sales web_q2_q3_increase
       ,ss3.store_sales/ss2.store_sales store_q2_q3_increase
 from
        ss ss1
       ,ss ss2
       ,ss ss3
       ,ws ws1
       ,ws ws2
       ,ws ws3
 where
    ss1.d_qoy = 1
    and ss1.d_year = 1999+1
    and ss1.ca_county = ss2.ca_county
    and ss2.d_qoy = 2
    and ss2.d_year = 1999+1
 and ss2.ca_county = ss3.ca_county
    and ss3.d_qoy = 3
    and ss3.d_year = 1999+1
    and ss1.ca_county = ws1.ca_county
    and ws1.d_qoy = 1
    and ws1.d_year = 1999+1
    and ws1.ca_county = ws2.ca_county
    and ws2.d_qoy = 2
    and ws2.d_year = 1999+1
    and ws1.ca_county = ws3.ca_county
    and ws3.d_qoy = 3
    and ws3.d_year =1999+1
    and case when ws1.web_sales > 0 then ws2.web_sales/ws1.web_sales else null end 
       > case when ss1.store_sales > 0 then ss2.store_sales/ss1.store_sales else null end
    and case when ws2.web_sales > 0 then ws3.web_sales/ws2.web_sales else null end
       > case when ss2.store_sales > 0 then ss3.store_sales/ss2.store_sales else null end
 order by ss1.ca_county;--

-- query32
select top 100 sum(cs_ext_discount_amt)  as "excess discount amount" 
from 
   catalog_sales 
   ,item 
   ,date_dim
where
i_manufact_id = 977
and i_item_sk = cs_item_sk 
and d_date between '2000-01-27' and 
        (cast('2000-04-26' as date))
and d_date_sk = cs_sold_date_sk 
and cs_ext_discount_amt  
     > ( 
         select 
            1.3 * avg(cs_ext_discount_amt) 
         from 
            catalog_sales 
           ,date_dim
         where 
              cs_item_sk = i_item_sk 
          and d_date between '2000-01-27' and
                             (cast('2000-04-26' as date))
          and d_date_sk = cs_sold_date_sk 
      ) ;--

-- query33
with ss as (
 select
          i_manufact_id,sum(ss_ext_sales_price) total_sales
 from
 	store_sales,
 	date_dim,
         customer_address,
         item
 where
         i_manufact_id in (select
  i_manufact_id
from
 item
where i_category in ('Electronics'))
 and     ss_item_sk              = i_item_sk
 and     ss_sold_date_sk         = d_date_sk
 and     d_year                  = 1998
 and     d_moy                   = 5
 and     ss_addr_sk              = ca_address_sk
 and     ca_gmt_offset           = -5 
 group by i_manufact_id),
 cs as (
 select
          i_manufact_id,sum(cs_ext_sales_price) total_sales
 from
 	catalog_sales,
 	date_dim,
         customer_address,
         item
 where
         i_manufact_id               in (select
  i_manufact_id
from
 item
where i_category in ('Electronics'))
 and     cs_item_sk              = i_item_sk
 and     cs_sold_date_sk         = d_date_sk
 and     d_year                  = 1998
 and     d_moy                   = 5
 and     cs_bill_addr_sk         = ca_address_sk
 and     ca_gmt_offset           = -5 
 group by i_manufact_id),
 ws as (
 select
          i_manufact_id,sum(ws_ext_sales_price) total_sales
 from
 	web_sales,
 	date_dim,
         customer_address,
         item
 where
         i_manufact_id               in (select
  i_manufact_id
from
 item
where i_category in ('Electronics'))
 and     ws_item_sk              = i_item_sk
 and     ws_sold_date_sk         = d_date_sk
 and     d_year                  = 1998
 and     d_moy                   = 5
 and     ws_bill_addr_sk         = ca_address_sk
 and     ca_gmt_offset           = -5
 group by i_manufact_id)
  select top 100 i_manufact_id ,sum(total_sales) total_sales
 from  (select * from ss 
        union all
        select * from cs 
        union all
        select * from ws) tmp1
 group by i_manufact_id
 order by total_sales;--

-- query34
 select c_last_name
       ,c_first_name
       ,c_salutation
       ,c_preferred_cust_flag
       ,ss_ticket_number
       ,cnt from
   (select ss_ticket_number
          ,ss_customer_sk
          ,count(*) cnt
    from store_sales,date_dim,store,household_demographics
    where store_sales.ss_sold_date_sk = date_dim.d_date_sk
    and store_sales.ss_store_sk = store.s_store_sk  
    and store_sales.ss_hdemo_sk = household_demographics.hd_demo_sk
    and (date_dim.d_dom between 1 and 3 or date_dim.d_dom between 25 and 28)
    and (household_demographics.hd_buy_potential = '>10000' or
         household_demographics.hd_buy_potential = 'Unknown')
    and household_demographics.hd_vehicle_count > 0
    and (case when household_demographics.hd_vehicle_count > 0 
	then household_demographics.hd_dep_count/ household_demographics.hd_vehicle_count 
	else null 
	end)  > 1.2
    and date_dim.d_year in (1999,1999+1,1999+2)
    and store.s_county in ('Williamson County','Williamson County','Williamson County','Williamson County',
                           'Williamson County','Williamson County','Williamson County','Williamson County')
    group by ss_ticket_number,ss_customer_sk) dn,customer
    where ss_customer_sk = c_customer_sk
      and cnt between 15 and 20
    order by c_last_name,c_first_name,c_salutation,c_preferred_cust_flag desc, ss_ticket_number;--
-- query35
SELECT TOP 100   ca_state,
  cd_gender,
  cd_marital_status,
  cd_dep_count,
  count(*) cnt1,
  MIN(cd_dep_count),
  MAX(cd_dep_count),
  AVG(cd_dep_count),
  cd_dep_employed_count,
  count(*) cnt2,
  MIN(cd_dep_employed_count),
  MAX(cd_dep_employed_count),
  AVG(cd_dep_employed_count),
  cd_dep_college_count,
  count(*) cnt3,
  MIN(cd_dep_college_count),
  MAX(cd_dep_college_count),
  AVG(cd_dep_college_count)
 from
  customer c,customer_address ca,customer_demographics
 where
  c.c_current_addr_sk = ca.ca_address_sk and
  cd_demo_sk = c.c_current_cdemo_sk and 
  exists (select *
          from store_sales,date_dim
          where c.c_customer_sk = ss_customer_sk and
                ss_sold_date_sk = d_date_sk and
                d_year = 2002 and
                d_qoy < 4) and
   (exists (select *
            from web_sales,date_dim
            where c.c_customer_sk = ws_bill_customer_sk and
                  ws_sold_date_sk = d_date_sk and
                  d_year = 2002 and
                  d_qoy < 4) or 
    exists (select * 
            from catalog_sales,date_dim
            where c.c_customer_sk = cs_ship_customer_sk and
                  cs_sold_date_sk = d_date_sk and
                  d_year = 2002 and
                  d_qoy < 4))
 group by ca_state,
          cd_gender,
          cd_marital_status,
          cd_dep_count,
          cd_dep_employed_count,
          cd_dep_college_count
 order by ca_state,
          cd_gender,
          cd_marital_status,
          cd_dep_count,
          cd_dep_employed_count,
          cd_dep_college_count;--

-- query36
select top 100 
    sum(ss_net_profit)/sum(ss_ext_sales_price) as gross_margin
   ,i_category
   ,i_class
   ,grouping(i_category)+grouping(i_class) as lochierarchy
   ,rank() over (
 	partition by grouping(i_category)+grouping(i_class),
 	case when grouping(i_class) = 0 then i_category end 
 	order by sum(ss_net_profit)/sum(ss_ext_sales_price) asc) as rank_within_parent
 from
    store_sales
   ,date_dim       d1
   ,item
   ,store
 where
    d1.d_year = 2001 
 and d1.d_date_sk = ss_sold_date_sk
 and i_item_sk  = ss_item_sk 
 and s_store_sk  = ss_store_sk
 and s_state in ('TN','TN','TN','TN',
                 'TN','TN','TN','TN')
 group by rollup(i_category,i_class)
 order by
   lochierarchy desc
  ,case when grouping(i_category)+grouping(i_class) = 0 then i_category end
  ,rank_within_parent;--

-- query37
select top 100 i_item_id
       ,i_item_desc
       ,i_current_price
 from item, inventory, date_dim, catalog_sales
 where i_current_price between 68 and 68 + 30
 and inv_item_sk = i_item_sk
 and d_date_sk=inv_date_sk
 and d_date between cast('2000-02-01' as date) and (dateadd(day,60,cast('2000-02-01' as date)))
 and i_manufact_id in (677,940,694,808)
 and inv_quantity_on_hand between 100 and 500
 and cs_item_sk = i_item_sk
 group by i_item_id,i_item_desc,i_current_price
 order by i_item_id;--

-- query38
select top 100 count(*) from (
    select distinct c_last_name, c_first_name, d_date
    from store_sales, date_dim, customer
          where store_sales.ss_sold_date_sk = date_dim.d_date_sk
      and store_sales.ss_customer_sk = customer.c_customer_sk
      and d_month_seq between 1200 and 1200 + 11
  intersect
    select distinct c_last_name, c_first_name, d_date
    from catalog_sales, date_dim, customer
          where catalog_sales.cs_sold_date_sk = date_dim.d_date_sk
      and catalog_sales.cs_bill_customer_sk = customer.c_customer_sk
      and d_month_seq between 1200 and 1200 + 11
  intersect
    select distinct c_last_name, c_first_name, d_date
    from web_sales, date_dim, customer
          where web_sales.ws_sold_date_sk = date_dim.d_date_sk
      and web_sales.ws_bill_customer_sk = customer.c_customer_sk
      and d_month_seq between 1200 and 1200 + 11
) hot_cust;--

-- query39
with inv as
(select w_warehouse_name,w_warehouse_sk,i_item_sk,d_moy
       ,stdev,mean, case mean when 0 then null else stdev/mean end cov
 from(select w_warehouse_name,w_warehouse_sk,i_item_sk,d_moy
            ,stdev(inv_quantity_on_hand) stdev,avg(inv_quantity_on_hand) mean
      from inventory
          ,item
          ,warehouse
          ,date_dim
      where inv_item_sk = i_item_sk
        and inv_warehouse_sk = w_warehouse_sk
        and inv_date_sk = d_date_sk
        and d_year =2001
      group by w_warehouse_name,w_warehouse_sk,i_item_sk,d_moy) foo
 where case mean when 0 then 0 else stdev/mean end > 1)
select inv1.w_warehouse_sk,inv1.i_item_sk,inv1.d_moy,inv1.mean, inv1.cov
        ,inv2.w_warehouse_sk,inv2.i_item_sk,inv2.d_moy,inv2.mean, inv2.cov
from inv inv1,inv inv2
where inv1.i_item_sk = inv2.i_item_sk
  and inv1.w_warehouse_sk =  inv2.w_warehouse_sk
  and inv1.d_moy=1
  and inv2.d_moy=1+1
order by inv1.w_warehouse_sk,inv1.i_item_sk,inv1.d_moy,inv1.mean,inv1.cov
        ,inv2.d_moy,inv2.mean, inv2.cov;--

-- query40
select top 100 
   w_state
  ,i_item_id
  ,sum(case when (cast(d_date as date) < cast ('2000-03-11' as date)) 
 		then cs_sales_price - coalesce(cr_refunded_cash,0) else 0 end) as sales_before
  ,sum(case when (cast(d_date as date) >= cast ('2000-03-1' as date)) 
 		then cs_sales_price - coalesce(cr_refunded_cash,0) else 0 end) as sales_after
 from
   catalog_sales left outer join catalog_returns on
       (cs_order_number = cr_order_number 
        and cs_item_sk = cr_item_sk)
  ,warehouse 
  ,item
  ,date_dim
 where
     i_current_price between 0.99 and 1.49
 and i_item_sk          = cs_item_sk
 and cs_warehouse_sk    = w_warehouse_sk 
 and cs_sold_date_sk    = d_date_sk
 and d_date between (dateadd(day, -30, cast ('2000-03-11' as date)))
                and (dateadd(day, 30, cast ('2000-03-11' as date)))
 group by
    w_state,i_item_id
 order by w_state,i_item_id;--

-- query41
select distinct top 100  i_product_name
 from item i1
 where i_manufact_id between 738 and 738+40 
   and (select count(*) as item_cnt
        from item
        where (i_manufact = i1.i_manufact and
        ((i_category = 'Women' and 
        (i_color = 'powder' or i_color = 'khaki') and 
        (i_units = 'Ounce' or i_units = 'Oz') and
        (i_size = 'medium' or i_size = 'extra large')
        ) or
        (i_category = 'Women' and
        (i_color = 'brown' or i_color = 'honeydew') and
        (i_units = 'Bunch' or i_units = 'Ton') and
        (i_size = 'N/A' or i_size = 'small')
        ) or
        (i_category = 'Men' and
        (i_color = 'floral' or i_color = 'deep') and
        (i_units = 'N/A' or i_units = 'Dozen') and
        (i_size = 'petite' or i_size = 'large')
        ) or
        (i_category = 'Men' and
        (i_color = 'light' or i_color = 'cornflower') and
        (i_units = 'Box' or i_units = 'Pound') and
        (i_size = 'medium' or i_size = 'extra large')
        ))) or
       (i_manufact = i1.i_manufact and
        ((i_category = 'Women' and 
        (i_color = 'midnight' or i_color = 'snow') and 
        (i_units = 'Pallet' or i_units = 'Gross') and
        (i_size = 'medium' or i_size = 'extra large')
        ) or
        (i_category = 'Women' and
        (i_color = 'cyan' or i_color = 'papaya') and
        (i_units = 'Cup' or i_units = 'Dram') and
        (i_size = 'N/A' or i_size = 'small')
        ) or
        (i_category = 'Men' and
        (i_color = 'orange' or i_color = 'frosted') and
        (i_units = 'Each' or i_units = 'Tbl') and
        (i_size = 'petite' or i_size = 'large')
        ) or
        (i_category = 'Men' and
        (i_color = 'forest' or i_color = 'ghost') and
        (i_units = 'Lb' or i_units = 'Bundle') and
        (i_size = 'medium' or i_size = 'extra large')
        )))) > 0
 order by i_product_name ;--


-- query42
select top 100 dt.d_year
 	,item.i_category_id
 	,item.i_category
 	,sum(ss_ext_sales_price)
 from 	date_dim dt
 	,store_sales
 	,item
 where dt.d_date_sk = store_sales.ss_sold_date_sk
 	and store_sales.ss_item_sk = item.i_item_sk
 	and item.i_manager_id = 1  	
 	and dt.d_moy=11
 	and dt.d_year=2000
 group by 	dt.d_year
 		,item.i_category_id
 		,item.i_category
 order by       sum(ss_ext_sales_price) desc,dt.d_year
 		,item.i_category_id
 		,item.i_category ;--


-- query43
SELECT TOP 100 s_store_name, 
               s_store_id, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Sunday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) sun_sales, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Monday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) mon_sales, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Tuesday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) tue_sales, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Wednesday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) wed_sales, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Thursday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) thu_sales, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Friday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) fri_sales, 
               Sum(CASE 
                     WHEN ( d_day_name = 'Saturday' ) THEN ss_sales_price 
                     ELSE NULL 
                   END) sat_sales 
FROM   date_dim, 
       store_sales, 
       store 
WHERE  d_date_sk = ss_sold_date_sk 
       AND s_store_sk = ss_store_sk 
       AND s_gmt_offset = -5 
       AND d_year = 2000
GROUP  BY s_store_name, 
          s_store_id 
ORDER  BY s_store_name, 
          s_store_id, 
          sun_sales, 
          mon_sales, 
          tue_sales, 
          wed_sales, 
          thu_sales, 
          fri_sales, 
          sat_sales;--

-- query44
select top 100 asceding.rnk, i1.i_product_name best_performing, i2.i_product_name worst_performing
from(select *
     from (select item_sk,rank() over (order by rank_col asc) rnk
           from (select ss_item_sk item_sk,avg(ss_net_profit) rank_col 
                 from store_sales ss1
                 where ss_store_sk = 4
                 group by ss_item_sk
                 having avg(ss_net_profit) > 0.9*(select avg(ss_net_profit) rank_col
                                                  from store_sales
                                                  where ss_store_sk = 4
                                                    and ss_addr_sk is null
                                                  group by ss_store_sk))V1)V11
     where rnk  < 11) asceding,
    (select *
     from (select item_sk,rank() over (order by rank_col desc) rnk
           from (select ss_item_sk item_sk,avg(ss_net_profit) rank_col
                 from store_sales ss1
                 where ss_store_sk = 4
                 group by ss_item_sk
                 having avg(ss_net_profit) > 0.9*(select avg(ss_net_profit) rank_col
                                                  from store_sales
                                                  where ss_store_sk = 4
                                                    and ss_addr_sk is null
                                                  group by ss_store_sk))V2)V21
     where rnk  < 11) descending,
item i1,
item i2
where asceding.rnk = descending.rnk 
  and i1.i_item_sk=asceding.item_sk
  and i2.i_item_sk=descending.item_sk
order by asceding.rnk;--


-- query45
select top 100 ca_zip, ca_city, sum(ws_sales_price)
 from web_sales, customer, customer_address, date_dim, item
 where ws_bill_customer_sk = c_customer_sk
 	and c_current_addr_sk = ca_address_sk 
 	and ws_item_sk = i_item_sk 
 	and ( SUBSTRING(ca_zip,1,5) in ('85669', '86197','88274','83405','86475', '85392', '85460', '80348', '81792')
 	      or 
 	      i_item_id in (select i_item_id
                             from item
                             where i_item_sk in (2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
                             )
 	    )
 	and ws_sold_date_sk = d_date_sk
 	and d_qoy = 2 and d_year = 2001
 group by ca_zip, ca_city
 order by ca_zip, ca_city ;--

-- query46
select top 100 c_last_name
       ,c_first_name
       ,ca_city
       ,bought_city
       ,ss_ticket_number
       ,amt,profit 
 from
   (select ss_ticket_number
          ,ss_customer_sk
          ,ca_city bought_city
          ,sum(ss_coupon_amt) amt
          ,sum(ss_net_profit) profit
    from store_sales,date_dim,store,household_demographics,customer_address 
    where store_sales.ss_sold_date_sk = date_dim.d_date_sk
    and store_sales.ss_store_sk = store.s_store_sk  
    and store_sales.ss_hdemo_sk = household_demographics.hd_demo_sk
    and store_sales.ss_addr_sk = customer_address.ca_address_sk
    and (household_demographics.hd_dep_count = 4 or
         household_demographics.hd_vehicle_count= 3)
    and date_dim.d_dow in (6,0)
    and date_dim.d_year in (2000-1,2000,2000+1) 
    and store.s_city in ('Fairview','Fairview','Fairview','Midway','Fairview') 
    group by ss_ticket_number,ss_customer_sk,ss_addr_sk,ca_city) dn,customer,customer_address current_addr
    where ss_customer_sk = c_customer_sk
      and customer.c_current_addr_sk = current_addr.ca_address_sk
      and current_addr.ca_city <> bought_city
  order by c_last_name
          ,c_first_name
          ,ca_city
          ,bought_city
          ,ss_ticket_number  ;--


-- query47
--  with v1 as(
--  select i_category, i_brand,
--         s_store_name, s_company_name,
--         d_year, d_moy,
--         sum(ss_sales_price) sum_sales,
--         avg(sum(ss_sales_price)) over
--           (partition by i_category, i_brand,
--                      s_store_name, s_company_name, d_year)
--           avg_monthly_sales,
--         rank() over
--           (partition by i_category, i_brand,
--                      s_store_name, s_company_name
--            order by d_year, d_moy) rn
--  from item, store_sales, date_dim, store
--  where ss_item_sk = i_item_sk and
--        ss_sold_date_sk = d_date_sk and
--        ss_store_sk = s_store_sk and
--        (
--          d_year = 1999 or
--          ( d_year = 1999-1 and d_moy =12) or
--          ( d_year = 1999+1 and d_moy =1)
--        )
--  group by i_category, i_brand,
--           s_store_name, s_company_name,
--           d_year, d_moy),
--  v2 as(
--  select v1.i_category, v1.i_brand, v1.s_store_name, v1.s_company_name
-- 		,v1.d_year, v1.d_moy
--         ,v1.avg_monthly_sales
--         ,v1.sum_sales, v1_lag.sum_sales psum, v1_lead.sum_sales nsum
--  from v1, v1 v1_lag, v1 v1_lead
--  where v1.i_category = v1_lag.i_category and
--        v1.i_category = v1_lead.i_category and
--        v1.i_brand = v1_lag.i_brand and
--        v1.i_brand = v1_lead.i_brand and
--        v1.s_store_name = v1_lag.s_store_name and
--        v1.s_store_name = v1_lead.s_store_name and
--        v1.s_company_name = v1_lag.s_company_name and
--        v1.s_company_name = v1_lead.s_company_name and
--        v1.rn = v1_lag.rn + 1 and
--        v1.rn = v1_lead.rn - 1)
--  SELECT top 100 *
--  from v2
--  where  d_year = 1999 and    
--         avg_monthly_sales > 0 and
--         case when avg_monthly_sales > 0 then abs(sum_sales - avg_monthly_sales) / avg_monthly_sales else null end > 0.1
--  order by sum_sales - avg_monthly_sales, s_store_name ;--

-- query48
select sum (ss_quantity)
 from store_sales, store, customer_demographics, customer_address, date_dim
 where s_store_sk = ss_store_sk
 and  ss_sold_date_sk = d_date_sk and d_year = 2000
 and  
 (
  (
   cd_demo_sk = ss_cdemo_sk
   and 
   cd_marital_status = 'M'
   and 
   cd_education_status = '4 yr Degree'
   and 
   ss_sales_price between 100.00 and 150.00  
   )
 or
  (
  cd_demo_sk = ss_cdemo_sk
   and 
   cd_marital_status = 'D'
   and 
   cd_education_status = '2 yr Degree'
   and 
   ss_sales_price between 50.00 and 100.00   
  )
 or 
 (
  cd_demo_sk = ss_cdemo_sk
  and 
   cd_marital_status = 'S'
   and 
   cd_education_status = 'College'
   and 
   ss_sales_price between 150.00 and 200.00  
 )
 )
 and
 (
  (
  ss_addr_sk = ca_address_sk
  and
  ca_country = 'United States'
  and
  ca_state in ('CO', 'OH', 'TX')
  and ss_net_profit between 0 and 2000  
  )
 or
  (ss_addr_sk = ca_address_sk
  and
  ca_country = 'United States'
  and
  ca_state in ('OR', 'MN', 'KY')
  and ss_net_profit between 150 and 3000 
  )
 or
  (ss_addr_sk = ca_address_sk
  and
  ca_country = 'United States'
  and
  ca_state in ('VA', 'CA', 'MS')
  and ss_net_profit between 50 and 25000 
  )
 );--

-- query49
SELECT TOP 100 'web' AS channel, 
               web.item, 
               web.return_ratio, 
               web.return_rank, 
               web.currency_rank 
FROM   (SELECT item, 
               return_ratio, 
               currency_ratio, 
               Rank() 
                 OVER ( 
                   ORDER BY return_ratio)   AS return_rank, 
               Rank() 
                 OVER ( 
                   ORDER BY currency_ratio) AS currency_rank 
        FROM   (SELECT ws.ws_item_sk AS item, 
                       ( Sum(COALESCE(wr.wr_return_quantity, 0)) / 
                         Sum(COALESCE(ws.ws_quantity, 0)) ) AS 
                       return_ratio, 
                       ( Sum(COALESCE(wr.wr_return_amt, 0)) / 
                         Sum(COALESCE(ws.ws_net_paid, 0)) ) AS currency_ratio 
                FROM   web_sales ws 
                       LEFT OUTER JOIN web_returns wr 
                                    ON ( ws.ws_order_number = wr.wr_order_number 
                                         AND ws.ws_item_sk = wr.wr_item_sk ), 
                       date_dim 
                WHERE  wr.wr_return_amt > 10000 
                       AND ws.ws_net_profit > 1 
                       AND ws.ws_net_paid > 0 
                       AND ws.ws_quantity > 0 
                       AND ws_sold_date_sk = d_date_sk 
                       AND d_year = 2001 
                       AND d_moy = 12 
                GROUP  BY ws.ws_item_sk) in_web) web 
WHERE  ( web.return_rank <= 10 
          OR web.currency_rank <= 10 ) 
UNION ALL 
SELECT 'catalog' AS channel, 
       catalog.item, 
       catalog.return_ratio, 
       catalog.return_rank, 
       catalog.currency_rank 
FROM   (SELECT item, 
               return_ratio, 
               currency_ratio, 
               Rank() 
                 OVER ( 
                   ORDER BY return_ratio)   AS return_rank, 
               Rank() 
                 OVER ( 
                   ORDER BY currency_ratio) AS currency_rank 
        FROM   (SELECT cs.cs_item_sk AS 
                       item, 
                       ( Sum(COALESCE(cr.cr_return_quantity, 0)) / 
                         Sum(COALESCE(cs.cs_quantity, 0)) ) AS 
                       return_ratio, 
                       ( Sum(COALESCE(cr.cr_return_amount, 0)) / 
                         Sum(COALESCE(cs.cs_net_paid, 0)) ) AS 
                       currency_ratio 
                FROM   catalog_sales cs 
                       LEFT OUTER JOIN catalog_returns cr 
                                    ON ( cs.cs_order_number = cr.cr_order_number 
                                         AND cs.cs_item_sk = cr.cr_item_sk ), 
                       date_dim 
                WHERE  cr.cr_return_amount > 10000 
                       AND cs.cs_net_profit > 1 
                       AND cs.cs_net_paid > 0 
                       AND cs.cs_quantity > 0 
                       AND cs_sold_date_sk = d_date_sk 
                       AND d_year = 2001 
                       AND d_moy = 12 
                GROUP  BY cs.cs_item_sk) in_cat) catalog 
WHERE  ( catalog.return_rank <= 10 
          OR catalog.currency_rank <= 10 ) 
UNION ALL
SELECT 'store' AS channel, 
       store.item, 
       store.return_ratio, 
       store.return_rank, 
       store.currency_rank 
FROM   (SELECT item, 
               return_ratio, 
               currency_ratio, 
               Rank() 
                 OVER ( 
                   ORDER BY return_ratio)   AS return_rank, 
               Rank() 
                 OVER ( 
                   ORDER BY currency_ratio) AS currency_rank 
        FROM   (SELECT sts.ss_item_sk AS 
                       item, 
                       ( Sum(COALESCE(sr.sr_return_quantity, 0)) / 
                         Sum(COALESCE(sts.ss_quantity, 0)) ) AS 
                       return_ratio, 
                       ( Sum(COALESCE(sr.sr_return_amt, 0)) 
                         / Sum(COALESCE(sts.ss_net_paid, 0)) ) AS 
                       currency_ratio 
                FROM   store_sales sts 
                       LEFT OUTER JOIN store_returns sr 
                                    ON ( sts.ss_ticket_number = 
                                         sr.sr_ticket_number 
                                         AND sts.ss_item_sk = sr.sr_item_sk ), 
                       date_dim 
                WHERE  sr.sr_return_amt > 10000 
                       AND sts.ss_net_profit > 1 
                       AND sts.ss_net_paid > 0 
                       AND sts.ss_quantity > 0 
                       AND ss_sold_date_sk = d_date_sk 
                       AND d_year = 2001
                       AND d_moy = 12 
                GROUP  BY sts.ss_item_sk) in_store) store 
WHERE  ( store.return_rank <= 10 
          OR store.currency_rank <= 10 ) 
ORDER  BY 1, 
          4, 
          5;--

-- query50
SELECT TOP 100 s_store_name, 
               s_company_id, 
               s_street_number, 
               s_street_name, 
               s_street_type, 
               s_suite_number, 
               s_city, 
               s_county, 
               s_state, 
               s_zip, 
               Sum(CASE 
                     WHEN ( sr_returned_date_sk - ss_sold_date_sk <= 30 ) THEN 1 
                     ELSE 0 
                   END) AS days_30, 
               Sum(CASE 
                     WHEN ( sr_returned_date_sk - ss_sold_date_sk > 30 ) 
                          AND ( sr_returned_date_sk - ss_sold_date_sk <= 60 ) 
                   THEN 1 
                     ELSE 0 
                   END) AS days_31_60, 
               Sum(CASE 
                     WHEN ( sr_returned_date_sk - ss_sold_date_sk > 60 ) 
                          AND ( sr_returned_date_sk - ss_sold_date_sk <= 90 ) 
                   THEN 1 
                     ELSE 0 
                   END) AS days_61_90, 
               Sum(CASE 
                     WHEN ( sr_returned_date_sk - ss_sold_date_sk > 90 ) 
                          AND ( sr_returned_date_sk - ss_sold_date_sk <= 120 ) 
                   THEN 1 
                     ELSE 0 
                   END) AS days_91_120, 
               Sum(CASE 
                     WHEN ( sr_returned_date_sk - ss_sold_date_sk > 120 ) THEN 1 
                     ELSE 0 
                   END) AS days_over_120 
FROM   store_sales, 
       store_returns, 
       store, 
       date_dim d1, 
       date_dim d2 
WHERE  d2.d_year = 2001 
       AND d2.d_moy = 8 
       AND ss_ticket_number = sr_ticket_number 
       AND ss_item_sk = sr_item_sk 
       AND ss_sold_date_sk = d1.d_date_sk 
       AND sr_returned_date_sk = d2.d_date_sk 
       AND ss_customer_sk = sr_customer_sk 
       AND ss_store_sk = s_store_sk 
GROUP  BY s_store_name, 
          s_company_id, 
          s_street_number, 
          s_street_name, 
          s_street_type, 
          s_suite_number, 
          s_city, 
          s_county, 
          s_state, 
          s_zip 
ORDER  BY s_store_name, 
          s_company_id, 
          s_street_number, 
          s_street_name, 
          s_street_type, 
          s_suite_number, 
          s_city, 
          s_county, 
          s_state, 
          s_zip;--

-- query51
WITH web_v1 AS 
( 
         SELECT   ws_item_sk item_sk, 
                  d_date, 
                  sum(Sum(ws_sales_price)) OVER (partition BY ws_item_sk ORDER BY d_date rows BETWEEN UNBOUNDED PRECEDING AND      CURRENT row) cume_sales
         FROM     web_sales , 
                  date_dim 
         WHERE    ws_sold_date_sk=d_date_sk 
         AND      d_month_seq BETWEEN 1200 AND      1200+11 
         AND      ws_item_sk IS NOT NULL 
         GROUP BY ws_item_sk, 
                  d_date), store_v1 AS 
( 
         SELECT   ss_item_sk item_sk, 
                  d_date, 
                  sum(sum(ss_sales_price)) OVER (partition BY ss_item_sk ORDER BY d_date rows BETWEEN UNBOUNDED PRECEDING AND      CURRENT row) cume_sales
         FROM     store_sales , 
                  date_dim 
         WHERE    ss_sold_date_sk=d_date_sk 
         AND      d_month_seq BETWEEN 1200 AND      1200+11 
         AND      ss_item_sk IS NOT NULL 
         GROUP BY ss_item_sk, 
                  d_date) 
SELECT TOP 100
         * 
FROM     ( 
                  SELECT   item_sk , 
                           d_date , 
                           web_sales , 
                           store_sales , 
                           max(web_sales) OVER (partition BY item_sk ORDER BY d_date rows BETWEEN UNBOUNDED PRECEDING AND      CURRENT row)   web_cumulative ,
                           max(store_sales) OVER (partition BY item_sk ORDER BY d_date rows BETWEEN UNBOUNDED PRECEDING AND      CURRENT row) store_cumulative
                  FROM     ( 
                                           SELECT 
                                                           CASE 
                                                                           WHEN web.item_sk IS NOT NULL THEN web.item_sk
                                                                           ELSE store.item_sk 
                                                           END item_sk , 
                                                           CASE 
                                                                           WHEN web.d_date IS NOT NULL THEN web.d_date
                                                                           ELSE store.d_date 
                                                           END              d_date , 
                                                           web.cume_sales   web_sales , 
                                                           store.cume_sales store_sales 
                                           FROM            web_v1 web 
                                           FULL OUTER JOIN store_v1 store 
                                           ON              ( 
                                                                           web.item_sk = store.item_sk
                                                           AND             web.d_date = store.d_date) )x )y
WHERE    web_cumulative > store_cumulative 
ORDER BY item_sk , 
         d_date ;--

-- query52
SELECT TOP 100 dt.d_year, 
               item.i_brand_id         brand_id, 
               item.i_brand            brand, 
               Sum(ss_ext_sales_price) ext_price 
FROM   date_dim dt, 
       store_sales, 
       item 
WHERE  dt.d_date_sk = store_sales.ss_sold_date_sk 
       AND store_sales.ss_item_sk = item.i_item_sk 
       AND item.i_manager_id = 1 
       AND dt.d_moy = 11 
       AND dt.d_year = 2000
GROUP  BY dt.d_year, 
          item.i_brand, 
          item.i_brand_id 
ORDER  BY dt.d_year, 
          ext_price DESC, 
          brand_id;--

-- query53
SELECT TOP 100 * 
FROM   (SELECT i_manufact_id, 
               Sum(ss_sales_price)             sum_sales, 
               Avg(Sum(ss_sales_price)) 
                 OVER ( 
                   partition BY i_manufact_id) avg_quarterly_sales 
        FROM   item, 
               store_sales, 
               date_dim, 
               store 
        WHERE  ss_item_sk = i_item_sk 
               AND ss_sold_date_sk = d_date_sk 
               AND ss_store_sk = s_store_sk 
               AND d_month_seq IN ( 1199 + 1, 1199 + 2, 1199 + 3, 
                                    1199 + 4, 1199 + 5, 1199 + 6, 1199 + 7, 
                                    1199 + 8, 1199 + 9, 1199 + 10, 1199 + 11 ,1199+12) 
               AND ( ( i_category IN ( 'Books', 'Children', 'Electronics' ) 
                       AND i_class IN ( 'personal', 'portable', 'reference', 
                                        'self-help' ) 
                       AND i_brand IN ( 'scholaramalgamalg #14', 
                                        'scholaramalgamalg #7' 
                                        , 
                                        'exportiunivamalg #9', 
                                                       'scholaramalgamalg #9' ) 
                     ) 
                      OR ( i_category IN ( 'Women', 'Music', 'Men' ) 
                           AND i_class IN ( 'accessories', 'classical', 
                                            'fragrances', 
                                            'pants' ) 
                           AND i_brand IN ( 'amalgimporto #1', 
                                            'edu packscholar #1', 
                                            'exportiimporto #1', 
                                                'importoamalg #1' ) ) ) 
        GROUP  BY i_manufact_id, 
                  d_qoy) tmp1 
WHERE  CASE 
         WHEN avg_quarterly_sales > 0 THEN Abs (sum_sales - avg_quarterly_sales) 
                                           / 
                                           avg_quarterly_sales 
         ELSE NULL 
       END > 0.1 
ORDER  BY avg_quarterly_sales, 
          sum_sales, 
          i_manufact_id
;--
-- query54
WITH my_customers 
     AS (SELECT DISTINCT c_customer_sk, 
                         c_current_addr_sk 
         FROM   (SELECT cs_sold_date_sk     sold_date_sk, 
                        cs_bill_customer_sk customer_sk, 
                        cs_item_sk          item_sk 
                 FROM   catalog_sales 
                 UNION ALL 
                 SELECT ws_sold_date_sk     sold_date_sk, 
                        ws_bill_customer_sk customer_sk, 
                        ws_item_sk          item_sk 
                 FROM   web_sales) cs_or_ws_sales, 
                item, 
                date_dim, 
                customer 
         WHERE  sold_date_sk = d_date_sk 
                AND item_sk = i_item_sk 
                AND i_category = 'Women' 
                AND i_class = 'maternity' 
                AND c_customer_sk = cs_or_ws_sales.customer_sk 
                AND d_moy = 12
                AND d_year = 1998), 
     my_revenue 
     AS (SELECT c_customer_sk, 
                Sum(ss_ext_sales_price) AS revenue 
         FROM   my_customers, 
                store_sales, 
                customer_address, 
                store, 
                date_dim 
         WHERE  c_current_addr_sk = ca_address_sk 
                AND ca_county = s_county 
                AND ca_state = s_state 
                AND ss_sold_date_sk = d_date_sk 
                AND c_customer_sk = ss_customer_sk 
                AND d_month_seq BETWEEN (SELECT DISTINCT d_month_seq + 1 
                                         FROM   date_dim 
                                         WHERE  d_year = 1998 
                                                AND d_moy = 12) AND 
                                        (SELECT DISTINCT 
                                        d_month_seq + 3 
                                         FROM   date_dim 
                                         WHERE  d_year = 1998 
                                                AND d_moy = 12) 
         GROUP  BY c_customer_sk), 
     segments 
     AS (SELECT Floor(revenue / 50) AS segment 
         FROM   my_revenue) 
SELECT TOP 100 segment, 
               Count(*)     AS num_customers, 
               segment * 50 AS segment_base 
FROM   segments 
GROUP  BY segment 
ORDER  BY segment, 
          num_customers
;--
-- query55
SELECT TOP 100 i_brand_id              brand_id, 
               i_brand                 brand, 
               Sum(ss_ext_sales_price) ext_price 
FROM   date_dim, 
       store_sales, 
       item 
WHERE  d_date_sk = ss_sold_date_sk 
       AND ss_item_sk = i_item_sk 
       AND i_manager_id = 28
       AND d_moy = 11
       AND d_year = 1999 
GROUP  BY i_brand, 
          i_brand_id 
ORDER  BY ext_price DESC, 
          i_brand_id
;--
-- query56
with ss as (
 select i_item_id,sum(ss_ext_sales_price) total_sales
 from
 	store_sales,
 	date_dim,
         customer_address,
         item
 where i_item_id in (select
     i_item_id
from item
where i_color in ('slate','blanched','burnished'))
 and     ss_item_sk              = i_item_sk
 and     ss_sold_date_sk         = d_date_sk
 and     d_year                  = 2001
 and     d_moy                   = 2
 and     ss_addr_sk              = ca_address_sk
 and     ca_gmt_offset           = -5 
 group by i_item_id),
 cs as (
 select i_item_id,sum(cs_ext_sales_price) total_sales
 from
 	catalog_sales,
 	date_dim,
         customer_address,
         item
 where
         i_item_id               in (select
  i_item_id
from item
where i_color in ('slate','blanched','burnished'))
 and     cs_item_sk              = i_item_sk
 and     cs_sold_date_sk         = d_date_sk
 and     d_year                  = 2001
 and     d_moy                   = 2
 and     cs_bill_addr_sk         = ca_address_sk
 and     ca_gmt_offset           = -5 
 group by i_item_id),
 ws as (
 select i_item_id,sum(ws_ext_sales_price) total_sales
 from
 	web_sales,
 	date_dim,
         customer_address,
         item
 where
         i_item_id               in (select
  i_item_id
from item
where i_color in ('slate','blanched','burnished'))
 and     ws_item_sk              = i_item_sk
 and     ws_sold_date_sk         = d_date_sk
 and     d_year                  = 2001
 and     d_moy                   = 2
 and     ws_bill_addr_sk         = ca_address_sk
 and     ca_gmt_offset           = -5
 group by i_item_id)
  select top 100 i_item_id ,sum(total_sales) total_sales
 from  (select * from ss 
        union all
        select * from cs 
        union all
        select * from ws) tmp1
 group by i_item_id
 order by total_sales,
          i_item_id
 ;--

-- query57
with v1 as(
 select i_category, i_brand,
        cc_name,
        d_year, d_moy,
        sum(cs_sales_price) sum_sales,
        avg(sum(cs_sales_price)) over
          (partition by i_category, i_brand,
                     cc_name, d_year)
          avg_monthly_sales,
        rank() over
          (partition by i_category, i_brand,
                     cc_name
           order by d_year, d_moy) rn
 from item, catalog_sales, date_dim, call_center
 where cs_item_sk = i_item_sk and
       cs_sold_date_sk = d_date_sk and
       cc_call_center_sk= cs_call_center_sk and
       (
         d_year = 1999 or
         ( d_year = 1999-1 and d_moy =12) or
         ( d_year = 1999+1 and d_moy =1)
       )
 group by i_category, i_brand,
          cc_name , d_year, d_moy),
 v2 as(
 select v1.i_category, v1.i_brand, v1.cc_name
      	,v1.d_year, v1.d_moy
        ,v1.avg_monthly_sales
        ,v1.sum_sales, v1_lag.sum_sales psum, v1_lead.sum_sales nsum
 from v1, v1 v1_lag, v1 v1_lead
 where v1.i_category = v1_lag.i_category and
       v1.i_category = v1_lead.i_category and
       v1.i_brand = v1_lag.i_brand and
       v1.i_brand = v1_lead.i_brand and
       v1. cc_name = v1_lag. cc_name and
       v1. cc_name = v1_lead. cc_name and
       v1.rn = v1_lag.rn + 1 and
       v1.rn = v1_lead.rn - 1)
select top 100 *
 from v2
 where  d_year = 1999 and
        avg_monthly_sales > 0 and
        case when avg_monthly_sales > 0 then abs(sum_sales - avg_monthly_sales) / avg_monthly_sales else null end > 0.1
 order by sum_sales - avg_monthly_sales, cc_name;--

-- query58
WITH ss_items 
     AS (SELECT i_item_id               item_id, 
                Sum(ss_ext_sales_price) ss_item_rev 
         FROM   store_sales, 
                item, 
                date_dim 
         WHERE  ss_item_sk = i_item_sk 
                AND d_date IN (SELECT d_date 
                               FROM   date_dim 
                               WHERE  d_week_seq = (SELECT d_week_seq 
                                                    FROM   date_dim 
                                                    WHERE  d_date = '2000-01-03' 
                                                   )) 
                AND ss_sold_date_sk = d_date_sk 
         GROUP  BY i_item_id), 
     cs_items 
     AS (SELECT i_item_id               item_id, 
                Sum(cs_ext_sales_price) cs_item_rev 
         FROM   catalog_sales, 
                item, 
                date_dim 
         WHERE  cs_item_sk = i_item_sk 
                AND d_date IN (SELECT d_date 
                               FROM   date_dim 
                               WHERE  d_week_seq = (SELECT d_week_seq 
                                                    FROM   date_dim 
                                                    WHERE  d_date = '2000-01-03' 
                                                   )) 
                AND cs_sold_date_sk = d_date_sk 
         GROUP  BY i_item_id), 
     ws_items 
     AS (SELECT i_item_id               item_id, 
                Sum(ws_ext_sales_price) ws_item_rev 
         FROM   web_sales, 
                item, 
                date_dim 
         WHERE  ws_item_sk = i_item_sk 
                AND d_date IN (SELECT d_date 
                               FROM   date_dim 
                               WHERE  d_week_seq = (SELECT d_week_seq 
                                                    FROM   date_dim 
                                                    WHERE  d_date = '2000-01-03' 
                                                   )) 
                AND ws_sold_date_sk = d_date_sk 
         GROUP  BY i_item_id) 
SELECT TOP 100 ss_items.item_id, 
               ss_item_rev, 
               ss_item_rev / ( ss_item_rev + cs_item_rev + ws_item_rev ) / 3 * 
               100 ss_dev, 
               cs_item_rev, 
               cs_item_rev / ( ss_item_rev + cs_item_rev + ws_item_rev ) / 3 * 
               100 cs_dev, 
               ws_item_rev, 
               ws_item_rev / ( ss_item_rev + cs_item_rev + ws_item_rev ) / 3 * 
               100 ws_dev, 
               ( ss_item_rev + cs_item_rev + ws_item_rev ) / 3 
               average 
FROM   ss_items, 
       cs_items, 
       ws_items 
WHERE  ss_items.item_id = cs_items.item_id 
       AND ss_items.item_id = ws_items.item_id 
       AND ss_item_rev BETWEEN 0.9 * cs_item_rev AND 1.1 * cs_item_rev 
       AND ss_item_rev BETWEEN 0.9 * ws_item_rev AND 1.1 * ws_item_rev 
       AND cs_item_rev BETWEEN 0.9 * ss_item_rev AND 1.1 * ss_item_rev 
       AND cs_item_rev BETWEEN 0.9 * ws_item_rev AND 1.1 * ws_item_rev 
       AND ws_item_rev BETWEEN 0.9 * ss_item_rev AND 1.1 * ss_item_rev 
       AND ws_item_rev BETWEEN 0.9 * cs_item_rev AND 1.1 * cs_item_rev 
ORDER  BY item_id, 
          ss_item_rev
;--
-- query59
WITH wss 
     AS (SELECT d_week_seq, 
                ss_store_sk, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Sunday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) sun_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Monday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) mon_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Tuesday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) tue_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Wednesday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) wed_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Thursday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) thu_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Friday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) fri_sales, 
                Sum(CASE 
                      WHEN ( d_day_name = 'Saturday' ) THEN ss_sales_price 
                      ELSE NULL 
                    END) sat_sales 
         FROM   store_sales, 
                date_dim 
         WHERE  d_date_sk = ss_sold_date_sk 
         GROUP  BY d_week_seq, 
                   ss_store_sk) 
SELECT TOP 100 s_store_name1, 
               s_store_id1, 
               d_week_seq1, 
               sun_sales1 / sun_sales2, 
               mon_sales1 / mon_sales2, 
               tue_sales1 / tue_sales2, 
               wed_sales1 / wed_sales2, 
               thu_sales1 / thu_sales2, 
               fri_sales1 / fri_sales2, 
               sat_sales1 / sat_sales2 
FROM   (SELECT s_store_name   s_store_name1, 
               wss.d_week_seq d_week_seq1, 
               s_store_id     s_store_id1, 
               sun_sales      sun_sales1, 
               mon_sales      mon_sales1, 
               tue_sales      tue_sales1, 
               wed_sales      wed_sales1, 
               thu_sales      thu_sales1, 
               fri_sales      fri_sales1, 
               sat_sales      sat_sales1 
        FROM   wss, 
               store, 
               date_dim d 
        WHERE  d.d_week_seq = wss.d_week_seq 
               AND ss_store_sk = s_store_sk 
               AND d_month_seq BETWEEN 1212 AND 1212 + 11) y, 
       (SELECT s_store_name   s_store_name2, 
               wss.d_week_seq d_week_seq2, 
               s_store_id     s_store_id2, 
               sun_sales      sun_sales2, 
               mon_sales      mon_sales2, 
               tue_sales      tue_sales2, 
               wed_sales      wed_sales2, 
               thu_sales      thu_sales2, 
               fri_sales      fri_sales2, 
               sat_sales      sat_sales2 
        FROM   wss, 
               store, 
               date_dim d 
        WHERE  d.d_week_seq = wss.d_week_seq 
               AND ss_store_sk = s_store_sk 
               AND d_month_seq BETWEEN 1212 + 12 AND 1212 + 23) x 
WHERE  s_store_id1 = s_store_id2 
       AND d_week_seq1 = d_week_seq2 - 52 
ORDER  BY s_store_name1, 
          s_store_id1, 
          d_week_seq1
;--
-- query60
WITH ss 
     AS (SELECT i_item_id, 
                Sum(ss_ext_sales_price) total_sales 
         FROM   store_sales, 
                date_dim, 
                customer_address, 
                item 
         WHERE  i_item_id IN (SELECT i_item_id 
                              FROM   item 
                              WHERE  i_category IN ( 'Music' )) 
                AND ss_item_sk = i_item_sk 
                AND ss_sold_date_sk = d_date_sk 
                AND d_year = 1998 
                AND d_moy = 9
                AND ss_addr_sk = ca_address_sk 
                AND ca_gmt_offset = -5 
         GROUP  BY i_item_id), 
     cs 
     AS (SELECT i_item_id, 
                Sum(cs_ext_sales_price) total_sales 
         FROM   catalog_sales, 
                date_dim, 
                customer_address, 
                item 
         WHERE  i_item_id IN (SELECT i_item_id 
                              FROM   item 
                              WHERE  i_category IN ( 'Music' )) 
                AND cs_item_sk = i_item_sk 
                AND cs_sold_date_sk = d_date_sk 
                AND d_year = 1998 
                AND d_moy = 9
                AND cs_bill_addr_sk = ca_address_sk 
                AND ca_gmt_offset = -5
         GROUP  BY i_item_id), 
     ws 
     AS (SELECT i_item_id, 
                Sum(ws_ext_sales_price) total_sales 
         FROM   web_sales, 
                date_dim, 
                customer_address, 
                item 
         WHERE  i_item_id IN (SELECT i_item_id 
                              FROM   item 
                              WHERE  i_category IN ( 'Music' )) 
                AND ws_item_sk = i_item_sk 
                AND ws_sold_date_sk = d_date_sk 
                AND d_year = 1998 
                AND d_moy = 9 
                AND ws_bill_addr_sk = ca_address_sk 
                AND ca_gmt_offset = -5 
         GROUP  BY i_item_id) 
SELECT TOP 100 i_item_id, 
               Sum(total_sales) total_sales 
FROM   (SELECT * 
        FROM   ss 
        UNION ALL 
        SELECT * 
        FROM   cs 
        UNION ALL 
        SELECT * 
        FROM   ws) tmp1 
GROUP  BY i_item_id 
ORDER  BY i_item_id, 
          total_sales
;--
-- query61
SELECT top 100 promotions,total,cast(promotions as decimal(15,4))/cast(total as decimal(15,4))*100
from
  (select sum(ss_ext_sales_price) promotions
   from  store_sales
        ,store
        ,promotion
        ,date_dim
        ,customer
        ,customer_address 
        ,item
   where ss_sold_date_sk = d_date_sk
   and   ss_store_sk = s_store_sk
   and   ss_promo_sk = p_promo_sk
   and   ss_customer_sk= c_customer_sk
   and   ca_address_sk = c_current_addr_sk
   and   ss_item_sk = i_item_sk 
   and   ca_gmt_offset = -5
   and   i_category = 'Jewelry'
   and   (p_channel_dmail = 'Y' or p_channel_email = 'Y' or p_channel_tv = 'Y')
   and   s_gmt_offset = -5
   and   d_year = 1998
   and   d_moy  = 11) promotional_sales,
  (select sum(ss_ext_sales_price) total
   from  store_sales
        ,store
        ,date_dim
        ,customer
        ,customer_address
        ,item
   where ss_sold_date_sk = d_date_sk
   and   ss_store_sk = s_store_sk
   and   ss_customer_sk= c_customer_sk
   and   ca_address_sk = c_current_addr_sk
   and   ss_item_sk = i_item_sk
   and   ca_gmt_offset = -5
   and   i_category = 'Jewelry'
   and   s_gmt_offset = -5
   and   d_year = 1998
   and   d_moy  = 11) all_sales
order by promotions, total

;--
-- query62
SELECT TOP 100 Substring(w_warehouse_name, 1, 20), 
               sm_type, 
               web_name, 
               Sum(CASE 
                     WHEN ( ws_ship_date_sk - ws_sold_date_sk <= 30 ) THEN 1 
                     ELSE 0 
                   END) AS days_30, 
               Sum(CASE 
                     WHEN ( ws_ship_date_sk - ws_sold_date_sk > 30 ) 
                          AND ( ws_ship_date_sk - ws_sold_date_sk <= 60 ) THEN 1 
                     ELSE 0 
                   END) AS days_31_60, 
               Sum(CASE 
                     WHEN ( ws_ship_date_sk - ws_sold_date_sk > 60 ) 
                          AND ( ws_ship_date_sk - ws_sold_date_sk <= 90 ) THEN 1 
                     ELSE 0 
                   END) AS days_61_90, 
               Sum(CASE 
                     WHEN ( ws_ship_date_sk - ws_sold_date_sk > 90 ) 
                          AND ( ws_ship_date_sk - ws_sold_date_sk <= 120 ) THEN 
                     1 
                     ELSE 0 
                   END) AS days_91_120, 
               Sum(CASE 
                     WHEN ( ws_ship_date_sk - ws_sold_date_sk > 120 ) THEN 1 
                     ELSE 0 
                   END) AS days_over_120 
FROM   web_sales, 
       warehouse, 
       ship_mode, 
       web_site, 
       date_dim 
WHERE  d_month_seq BETWEEN 1200 AND 1200 + 11 
       AND ws_ship_date_sk = d_date_sk 
       AND ws_warehouse_sk = w_warehouse_sk 
       AND ws_ship_mode_sk = sm_ship_mode_sk 
       AND ws_web_site_sk = web_site_sk 
GROUP  BY Substring(w_warehouse_name, 1, 20), 
          sm_type, 
          web_name 
ORDER  BY 1, 
          sm_type, 
          web_name;--

-- query63
SELECT TOP 100 * 
FROM   (SELECT i_manager_id, 
               Sum(ss_sales_price)            sum_sales, 
               Avg(Sum(ss_sales_price)) 
                 OVER ( 
                   partition BY i_manager_id) avg_monthly_sales 
        FROM   item, 
               store_sales, 
               date_dim, 
               store 
        WHERE  ss_item_sk = i_item_sk 
               AND ss_sold_date_sk = d_date_sk 
               AND ss_store_sk = s_store_sk 
               AND d_month_seq IN ( 1200, 1200 + 1, 1200 + 2, 1200 + 3, 
                                    1200 + 4, 1200 + 5, 1200 + 6, 1200 + 7, 
                                    1200 + 8, 1200 + 9, 1200 + 10, 1200 + 11 ) 
               AND ( ( i_category IN ( 'Books', 'Children', 'Electronics' ) 
                       AND i_class IN ( 'personal', 'portable', 'reference', 
                                        'self-help' ) 
                       AND i_brand IN ( 'scholaramalgamalg #14', 
                                        'scholaramalgamalg #7' 
                                        , 
                                        'exportiunivamalg #9', 
                                                       'scholaramalgamalg #9' ) 
                     ) 
                      OR ( i_category IN ( 'Women', 'Music', 'Men' ) 
                           AND i_class IN ( 'accessories', 'classical', 
                                            'fragrances', 
                                            'pants' ) 
                           AND i_brand IN ( 'amalgimporto #1', 
                                            'edu packscholar #1', 
                                            'exportiimporto #1', 
                                                'importoamalg #1' ) ) ) 
        GROUP  BY i_manager_id, 
                  d_moy) tmp1 
WHERE  CASE 
         WHEN avg_monthly_sales > 0 THEN Abs (sum_sales - avg_monthly_sales) / 
                                         avg_monthly_sales 
         ELSE NULL 
       END > 0.1 
ORDER  BY i_manager_id, 
          avg_monthly_sales, 
          sum_sales;--


-- query64
WITH cs_ui 
     AS (SELECT cs_item_sk, 
                Sum(cs_ext_list_price) AS sale, 
                Sum(cr_refunded_cash + cr_reversed_charge 
                    + cr_store_credit) AS refund 
         FROM   catalog_sales, 
                catalog_returns 
         WHERE  cs_item_sk = cr_item_sk 
                AND cs_order_number = cr_order_number 
         GROUP  BY cs_item_sk 
         HAVING Sum(cs_ext_list_price) > 2 * Sum( 
                cr_refunded_cash + cr_reversed_charge 
                + cr_store_credit)), 
     cross_sales 
     AS (SELECT i_product_name         product_name, 
                i_item_sk              item_sk, 
                s_store_name           store_name, 
                s_zip                  store_zip, 
                ad1.ca_street_number   b_street_number, 
                ad1.ca_street_name     b_streen_name, 
                ad1.ca_city            b_city, 
                ad1.ca_zip             b_zip, 
                ad2.ca_street_number   c_street_number, 
                ad2.ca_street_name     c_street_name, 
                ad2.ca_city            c_city, 
                ad2.ca_zip             c_zip, 
                d1.d_year              AS syear, 
                d2.d_year              AS fsyear, 
                d3.d_year              s2year, 
                Count(*)               cnt, 
                Sum(ss_wholesale_cost) s1, 
                Sum(ss_list_price)     s2, 
                Sum(ss_coupon_amt)     s3 
         FROM   store_sales, 
                store_returns, 
                cs_ui, 
                date_dim d1, 
                date_dim d2, 
                date_dim d3, 
                store, 
                customer, 
                customer_demographics cd1, 
                customer_demographics cd2, 
                promotion, 
                household_demographics hd1, 
                household_demographics hd2, 
                customer_address ad1, 
                customer_address ad2, 
                income_band ib1, 
                income_band ib2, 
                item 
         WHERE  ss_store_sk = s_store_sk 
                AND ss_sold_date_sk = d1.d_date_sk 
                AND ss_customer_sk = c_customer_sk 
                AND ss_cdemo_sk = cd1.cd_demo_sk 
                AND ss_hdemo_sk = hd1.hd_demo_sk 
                AND ss_addr_sk = ad1.ca_address_sk 
                AND ss_item_sk = i_item_sk 
                AND ss_item_sk = sr_item_sk 
                AND ss_ticket_number = sr_ticket_number 
                AND ss_item_sk = cs_ui.cs_item_sk 
                AND c_current_cdemo_sk = cd2.cd_demo_sk 
                AND c_current_hdemo_sk = hd2.hd_demo_sk 
                AND c_current_addr_sk = ad2.ca_address_sk 
                AND c_first_sales_date_sk = d2.d_date_sk 
                AND c_first_shipto_date_sk = d3.d_date_sk 
                AND ss_promo_sk = p_promo_sk 
                AND hd1.hd_income_band_sk = ib1.ib_income_band_sk 
                AND hd2.hd_income_band_sk = ib2.ib_income_band_sk 
                AND cd1.cd_marital_status <> cd2.cd_marital_status 
                AND i_color IN ( 'purple', 'burlywood','indian','spring','floral','medium') 
                AND i_current_price BETWEEN 64 AND 64 + 10 
                AND i_current_price BETWEEN 64 + 1 AND 64 + 15 
         GROUP  BY i_product_name, 
                   i_item_sk, 
                   s_store_name, 
                   s_zip, 
                   ad1.ca_street_number, 
                   ad1.ca_street_name, 
                   ad1.ca_city, 
                   ad1.ca_zip, 
                   ad2.ca_street_number, 
                   ad2.ca_street_name, 
                   ad2.ca_city, 
                   ad2.ca_zip, 
                   d1.d_year, 
                   d2.d_year, 
                   d3.d_year) 
SELECT cs1.product_name, 
       cs1.store_name, 
       cs1.store_zip, 
       cs1.b_street_number, 
       cs1.b_streen_name, 
       cs1.b_city, 
       cs1.b_zip, 
       cs1.c_street_number, 
       cs1.c_street_name, 
       cs1.c_city, 
       cs1.c_zip, 
       cs1.syear, 
       cs1.cnt, 
       cs1.s1, 
       cs1.s2, 
       cs1.s3, 
       cs2.s1 AS s1_, 
       cs2.s2 AS s2_, 
       cs2.s3 AS s3_, 
       cs2.syear AS syear_, 
       cs2.cnt AS cnt_ 
FROM   cross_sales cs1, 
       cross_sales cs2 
WHERE  cs1.item_sk = cs2.item_sk 
       AND cs1.syear = 1999
       AND cs2.syear = 1999 + 1 
       AND cs2.cnt <= cs1.cnt 
       AND cs1.store_name = cs2.store_name 
       AND cs1.store_zip = cs2.store_zip 
ORDER  BY cs1.product_name, 
          cs1.store_name, 
          cs2.cnt;-- 


-- query65
SELECT TOP 100 s_store_name, 
               i_item_desc, 
               sc.revenue, 
               i_current_price, 
               i_wholesale_cost, 
               i_brand 
FROM   store, 
       item, 
       (SELECT ss_store_sk, 
               Avg(revenue) AS ave 
        FROM   (SELECT ss_store_sk, 
                       ss_item_sk, 
                       Sum(ss_sales_price) AS revenue 
                FROM   store_sales, 
                       date_dim 
                WHERE  ss_sold_date_sk = d_date_sk 
                       AND d_month_seq BETWEEN 1176 AND 1176 + 11 
                GROUP  BY ss_store_sk, 
                          ss_item_sk) sa 
        GROUP  BY ss_store_sk) sb, 
       (SELECT ss_store_sk, 
               ss_item_sk, 
               Sum(ss_sales_price) AS revenue 
        FROM   store_sales, 
               date_dim 
        WHERE  ss_sold_date_sk = d_date_sk 
               AND d_month_seq BETWEEN 1176 AND 1176 + 11 
        GROUP  BY ss_store_sk, 
                  ss_item_sk) sc 
WHERE  sb.ss_store_sk = sc.ss_store_sk 
       AND sc.revenue <= 0.1 * sb.ave 
       AND s_store_sk = sc.ss_store_sk 
       AND i_item_sk = sc.ss_item_sk 
ORDER  BY s_store_name, 
          i_item_desc
;--

-- query66
SELECT TOP 100 
     w_warehouse_name
 	,w_warehouse_sq_ft
 	,w_city
 	,w_county
 	,w_state
 	,w_country
        ,ship_carriers
        ,year
 	,sum(jan_sales) as jan_sales
 	,sum(feb_sales) as feb_sales
 	,sum(mar_sales) as mar_sales
 	,sum(apr_sales) as apr_sales
 	,sum(may_sales) as may_sales
 	,sum(jun_sales) as jun_sales
 	,sum(jul_sales) as jul_sales
 	,sum(aug_sales) as aug_sales
 	,sum(sep_sales) as sep_sales
 	,sum(oct_sales) as oct_sales
 	,sum(nov_sales) as nov_sales
 	,sum(dec_sales) as dec_sales
 	,sum(jan_sales/w_warehouse_sq_ft) as jan_sales_per_sq_foot
 	,sum(feb_sales/w_warehouse_sq_ft) as feb_sales_per_sq_foot
 	,sum(mar_sales/w_warehouse_sq_ft) as mar_sales_per_sq_foot
 	,sum(apr_sales/w_warehouse_sq_ft) as apr_sales_per_sq_foot
 	,sum(may_sales/w_warehouse_sq_ft) as may_sales_per_sq_foot
 	,sum(jun_sales/w_warehouse_sq_ft) as jun_sales_per_sq_foot
 	,sum(jul_sales/w_warehouse_sq_ft) as jul_sales_per_sq_foot
 	,sum(aug_sales/w_warehouse_sq_ft) as aug_sales_per_sq_foot
 	,sum(sep_sales/w_warehouse_sq_ft) as sep_sales_per_sq_foot
 	,sum(oct_sales/w_warehouse_sq_ft) as oct_sales_per_sq_foot
 	,sum(nov_sales/w_warehouse_sq_ft) as nov_sales_per_sq_foot
 	,sum(dec_sales/w_warehouse_sq_ft) as dec_sales_per_sq_foot
 	,sum(jan_net) as jan_net
 	,sum(feb_net) as feb_net
 	,sum(mar_net) as mar_net
 	,sum(apr_net) as apr_net
 	,sum(may_net) as may_net
 	,sum(jun_net) as jun_net
 	,sum(jul_net) as jul_net
 	,sum(aug_net) as aug_net
 	,sum(sep_net) as sep_net
 	,sum(oct_net) as oct_net
 	,sum(nov_net) as nov_net
 	,sum(dec_net) as dec_net
 from (
     select 
 	w_warehouse_name
 	,w_warehouse_sq_ft
 	,w_city
 	,w_county
 	,w_state
 	,w_country
 	,'DHL' + ',' + 'BARIAN' as ship_carriers
       ,d_year as year
 	,sum(case when d_moy = 1 
 		then ws_ext_sales_price* ws_quantity else 0 end) as jan_sales
 	,sum(case when d_moy = 2 
 		then ws_ext_sales_price* ws_quantity else 0 end) as feb_sales
 	,sum(case when d_moy = 3 
 		then ws_ext_sales_price* ws_quantity else 0 end) as mar_sales
 	,sum(case when d_moy = 4 
 		then ws_ext_sales_price* ws_quantity else 0 end) as apr_sales
 	,sum(case when d_moy = 5 
 		then ws_ext_sales_price* ws_quantity else 0 end) as may_sales
 	,sum(case when d_moy = 6 
 		then ws_ext_sales_price* ws_quantity else 0 end) as jun_sales
 	,sum(case when d_moy = 7 
 		then ws_ext_sales_price* ws_quantity else 0 end) as jul_sales
 	,sum(case when d_moy = 8 
 		then ws_ext_sales_price* ws_quantity else 0 end) as aug_sales
 	,sum(case when d_moy = 9 
 		then ws_ext_sales_price* ws_quantity else 0 end) as sep_sales
 	,sum(case when d_moy = 10 
 		then ws_ext_sales_price* ws_quantity else 0 end) as oct_sales
 	,sum(case when d_moy = 11
 		then ws_ext_sales_price* ws_quantity else 0 end) as nov_sales
 	,sum(case when d_moy = 12
 		then ws_ext_sales_price* ws_quantity else 0 end) as dec_sales
 	,sum(case when d_moy = 1 
 		then ws_net_paid * ws_quantity else 0 end) as jan_net
 	,sum(case when d_moy = 2
 		then ws_net_paid * ws_quantity else 0 end) as feb_net
 	,sum(case when d_moy = 3 
 		then ws_net_paid * ws_quantity else 0 end) as mar_net
 	,sum(case when d_moy = 4 
 		then ws_net_paid * ws_quantity else 0 end) as apr_net
 	,sum(case when d_moy = 5 
 		then ws_net_paid * ws_quantity else 0 end) as may_net
 	,sum(case when d_moy = 6 
 		then ws_net_paid * ws_quantity else 0 end) as jun_net
 	,sum(case when d_moy = 7 
 		then ws_net_paid * ws_quantity else 0 end) as jul_net
 	,sum(case when d_moy = 8 
 		then ws_net_paid * ws_quantity else 0 end) as aug_net
 	,sum(case when d_moy = 9 
 		then ws_net_paid * ws_quantity else 0 end) as sep_net
 	,sum(case when d_moy = 10 
 		then ws_net_paid * ws_quantity else 0 end) as oct_net
 	,sum(case when d_moy = 11
 		then ws_net_paid * ws_quantity else 0 end) as nov_net
 	,sum(case when d_moy = 12
 		then ws_net_paid * ws_quantity else 0 end) as dec_net
     from
          web_sales
         ,warehouse
         ,date_dim
         ,time_dim
 	  ,ship_mode
     where
            ws_warehouse_sk =  w_warehouse_sk
        and ws_sold_date_sk = d_date_sk
        and ws_sold_time_sk = t_time_sk
 	and ws_ship_mode_sk = sm_ship_mode_sk
        and d_year = 2001
 	and t_time between 30838 and 30838+28800 
 	and sm_carrier in ('DHL','BARIAN')
     group by 
        w_warehouse_name
 	,w_warehouse_sq_ft
 	,w_city
 	,w_county
 	,w_state
 	,w_country
       ,d_year
 union all
     select 
 	w_warehouse_name
 	,w_warehouse_sq_ft
 	,w_city
 	,w_county
 	,w_state
 	,w_country
 	,'DHL' + ',' + 'BARIAN' as ship_carriers
       ,d_year as year
 	,sum(case when d_moy = 1 
 		then cs_sales_price* cs_quantity else 0 end) as jan_sales
 	,sum(case when d_moy = 2 
 		then cs_sales_price* cs_quantity else 0 end) as feb_sales
 	,sum(case when d_moy = 3 
 		then cs_sales_price* cs_quantity else 0 end) as mar_sales
 	,sum(case when d_moy = 4 
 		then cs_sales_price* cs_quantity else 0 end) as apr_sales
 	,sum(case when d_moy = 5 
 		then cs_sales_price* cs_quantity else 0 end) as may_sales
 	,sum(case when d_moy = 6 
 		then cs_sales_price* cs_quantity else 0 end) as jun_sales
 	,sum(case when d_moy = 7 
 		then cs_sales_price* cs_quantity else 0 end) as jul_sales
 	,sum(case when d_moy = 8 
 		then cs_sales_price* cs_quantity else 0 end) as aug_sales
 	,sum(case when d_moy = 9 
 		then cs_sales_price* cs_quantity else 0 end) as sep_sales
 	,sum(case when d_moy = 10 
 		then cs_sales_price* cs_quantity else 0 end) as oct_sales
 	,sum(case when d_moy = 11
 		then cs_sales_price* cs_quantity else 0 end) as nov_sales
 	,sum(case when d_moy = 12
 		then cs_sales_price* cs_quantity else 0 end) as dec_sales
 	,sum(case when d_moy = 1 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as jan_net
 	,sum(case when d_moy = 2 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as feb_net
 	,sum(case when d_moy = 3 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as mar_net
 	,sum(case when d_moy = 4 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as apr_net
 	,sum(case when d_moy = 5 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as may_net
 	,sum(case when d_moy = 6 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as jun_net
 	,sum(case when d_moy = 7 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as jul_net
 	,sum(case when d_moy = 8 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as aug_net
 	,sum(case when d_moy = 9 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as sep_net
 	,sum(case when d_moy = 10 
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as oct_net
 	,sum(case when d_moy = 11
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as nov_net
 	,sum(case when d_moy = 12
 		then cs_net_paid_inc_tax * cs_quantity else 0 end) as dec_net
     from
          catalog_sales
         ,warehouse
         ,date_dim
         ,time_dim
 	 ,ship_mode
     where
            cs_warehouse_sk =  w_warehouse_sk
        and cs_sold_date_sk = d_date_sk
        and cs_sold_time_sk = t_time_sk
 	and cs_ship_mode_sk = sm_ship_mode_sk
        and d_year = 2001
 	and t_time between 30838 AND 30838+28800 
 	and sm_carrier in ('DHL','BARIAN')
     group by 
        w_warehouse_name
 	,w_warehouse_sq_ft
 	,w_city
 	,w_county
 	,w_state
 	,w_country
       ,d_year
 ) x
 group by 
        w_warehouse_name
 	,w_warehouse_sq_ft
 	,w_city
 	,w_county
 	,w_state
 	,w_country
 	,ship_carriers
       ,year
 order by w_warehouse_name
;--
-- query67
-- select top 100 *
-- from (select i_category
--             ,i_class
--             ,i_brand
--             ,i_product_name
--             ,d_year
--             ,d_qoy
--             ,d_moy
--             ,s_store_id
--             ,sumsales
--             ,rank() over (partition by i_category order by sumsales desc) rk
--       from (select i_category
--                   ,i_class
--                   ,i_brand
--                   ,i_product_name
--                   ,d_year
--                   ,d_qoy
--                   ,d_moy
--                   ,s_store_id
--                   ,sum(coalesce(ss_sales_price*ss_quantity,0)) sumsales
--             from store_sales
--                 ,date_dim
--                 ,store
--                 ,item
--        where  ss_sold_date_sk=d_date_sk
--           and ss_item_sk=i_item_sk
--           and ss_store_sk = s_store_sk
--           and d_month_seq between 1200 and 1200+11
--        group by  rollup(i_category, i_class, i_brand, i_product_name, d_year, d_qoy, d_moy,s_store_id))dw1) dw2
-- where rk <= 100
-- order by i_category
--         ,i_class
--         ,i_brand
--         ,i_product_name
--         ,d_year
--         ,d_qoy
--         ,d_moy
--         ,s_store_id
--         ,sumsales
--         ,rk
-- ;--

-- query68
SELECT TOP 100 c_last_name, 
               c_first_name, 
               ca_city, 
               bought_city, 
               ss_ticket_number, 
               extended_price, 
               extended_tax, 
               list_price 
FROM   (SELECT ss_ticket_number, 
               ss_customer_sk, 
               ca_city                 bought_city, 
               Sum(ss_ext_sales_price) extended_price, 
               Sum(ss_ext_list_price)  list_price, 
               Sum(ss_ext_tax)         extended_tax 
        FROM   store_sales, 
               date_dim, 
               store, 
               household_demographics, 
               customer_address 
        WHERE  store_sales.ss_sold_date_sk = date_dim.d_date_sk 
               AND store_sales.ss_store_sk = store.s_store_sk 
               AND store_sales.ss_hdemo_sk = household_demographics.hd_demo_sk 
               AND store_sales.ss_addr_sk = customer_address.ca_address_sk 
               AND date_dim.d_dom BETWEEN 1 AND 2 
               AND ( household_demographics.hd_dep_count = 4 
                      OR household_demographics.hd_vehicle_count = 3 ) 
               AND date_dim.d_year IN ( 1999, 1999 + 1, 1999 + 2 ) 
               AND store.s_city IN ( 'Midway', 'Fairview') 
        GROUP  BY ss_ticket_number, 
                  ss_customer_sk, 
                  ss_addr_sk, 
                  ca_city) dn, 
       customer, 
       customer_address current_addr 
WHERE  ss_customer_sk = c_customer_sk 
       AND customer.c_current_addr_sk = current_addr.ca_address_sk 
       AND current_addr.ca_city <> bought_city 
ORDER  BY c_last_name, 
          ss_ticket_number;--

-- query69
SELECT TOP 100 cd_gender, 
               cd_marital_status, 
               cd_education_status, 
               Count(*) cnt1, 
               cd_purchase_estimate, 
               Count(*) cnt2, 
               cd_credit_rating, 
               Count(*) cnt3 
FROM   customer c, 
       customer_address ca, 
       customer_demographics 
WHERE  c.c_current_addr_sk = ca.ca_address_sk 
       AND ca_state IN ( 'KY', 'GA', 'NM' ) 
       AND cd_demo_sk = c.c_current_cdemo_sk 
       AND EXISTS (SELECT * 
                   FROM   store_sales, 
                          date_dim 
                   WHERE  c.c_customer_sk = ss_customer_sk 
                          AND ss_sold_date_sk = d_date_sk 
                          AND d_year = 2001
                          AND d_moy BETWEEN 4 AND 4 + 2) 
       AND ( NOT EXISTS (SELECT * 
                         FROM   web_sales, 
                                date_dim 
                         WHERE  c.c_customer_sk = ws_bill_customer_sk 
                                AND ws_sold_date_sk = d_date_sk 
                                AND d_year = 2001 
                                AND d_moy BETWEEN 4 AND 4 + 2) 
             AND NOT EXISTS (SELECT * 
                             FROM   catalog_sales, 
                                    date_dim 
                             WHERE  c.c_customer_sk = cs_ship_customer_sk 
                                    AND cs_sold_date_sk = d_date_sk 
                                    AND d_year = 2001 
                                    AND d_moy BETWEEN 4 AND 4 + 2) ) 
GROUP  BY cd_gender, 
          cd_marital_status, 
          cd_education_status, 
          cd_purchase_estimate, 
          cd_credit_rating 
ORDER  BY cd_gender, 
          cd_marital_status, 
          cd_education_status, 
          cd_purchase_estimate, 
          cd_credit_rating;--

-- query70
select top 100 
    sum(ss_net_profit) as total_sum
   ,s_state
   ,s_county
   ,grouping(s_state)+grouping(s_county) as lochierarchy
   ,rank() over (
 	partition by grouping(s_state)+grouping(s_county),
 	case when grouping(s_county) = 0 then s_state end 
 	order by sum(ss_net_profit) desc) as rank_within_parent
 from
    store_sales
   ,date_dim       d1
   ,store
 where
    d1.d_month_seq between 1200 and 1200+11
 and d1.d_date_sk = ss_sold_date_sk
 and s_store_sk  = ss_store_sk
 and s_state in
             ( select s_state
               from  (select s_state as s_state,
 			    rank() over ( partition by s_state order by sum(ss_net_profit) desc) as ranking
                      from   store_sales, store, date_dim
                      where  d_month_seq between 1200 and 1200+11
 			    and d_date_sk = ss_sold_date_sk
 			    and s_store_sk  = ss_store_sk
                      group by s_state
                     ) tmp1 
               where ranking <= 5
             )
 group by rollup(s_state,s_county)
 order by
   lochierarchy desc
  ,case when grouping(s_state)+grouping(s_county) = 0 then s_state end
  ,rank_within_parent
 ;--
-- query71
SELECT i_brand_id     brand_id, 
       i_brand        brand, 
       t_hour, 
       t_minute, 
       Sum(ext_price) ext_price 
FROM   item, 
       (SELECT ws_ext_sales_price AS ext_price, 
               ws_sold_date_sk    AS sold_date_sk, 
               ws_item_sk         AS sold_item_sk, 
               ws_sold_time_sk    AS time_sk 
        FROM   web_sales, 
               date_dim 
        WHERE  d_date_sk = ws_sold_date_sk 
               AND d_moy = 11 
               AND d_year = 1999 
        UNION ALL 
        SELECT cs_ext_sales_price AS ext_price, 
               cs_sold_date_sk    AS sold_date_sk, 
               cs_item_sk         AS sold_item_sk, 
               cs_sold_time_sk    AS time_sk 
        FROM   catalog_sales, 
               date_dim 
        WHERE  d_date_sk = cs_sold_date_sk 
               AND d_moy = 11 
               AND d_year = 1999 
        UNION ALL 
        SELECT ss_ext_sales_price AS ext_price, 
               ss_sold_date_sk    AS sold_date_sk, 
               ss_item_sk         AS sold_item_sk, 
               ss_sold_time_sk    AS time_sk 
        FROM   store_sales, 
               date_dim 
        WHERE  d_date_sk = ss_sold_date_sk 
               AND d_moy = 11 
               AND d_year = 1999) AS tmp, 
       time_dim 
WHERE  sold_item_sk = i_item_sk 
       AND i_manager_id = 1 
       AND time_sk = t_time_sk 
       AND ( t_meal_time = 'breakfast' 
              OR t_meal_time = 'dinner' ) 
GROUP  BY i_brand, 
          i_brand_id, 
          t_hour, 
          t_minute 
ORDER  BY ext_price DESC, 
          i_brand_id;-- 

-- query72
WITH top_items AS (
  SELECT cs_item_sk, 
                cs_warehouse_sk, 
                d1.d_week_seq, 
                Sum(CASE 
                      WHEN p_promo_sk IS NULL THEN 1 
                      ELSE 0 
                    END) no_promo, 
                Sum(CASE 
                      WHEN p_promo_sk IS NOT NULL THEN 1 
                      ELSE 0 
                    END) promo, 
                Count(*) total_cnt 
  FROM   catalog_sales 
        JOIN inventory 
          ON ( cs_item_sk = inv_item_sk and cs_sold_date_sk = inv_date_sk and cs_warehouse_sk = inv_warehouse_sk ) 
        JOIN customer_demographics 
          ON ( cs_bill_cdemo_sk = cd_demo_sk ) 
        JOIN household_demographics 
          ON ( cs_bill_hdemo_sk = hd_demo_sk ) 
        JOIN date_dim d1 
          ON ( cs_sold_date_sk = d1.d_date_sk ) 
        LEFT OUTER JOIN promotion 
                      ON ( cs_promo_sk = p_promo_sk ) 
        LEFT OUTER JOIN catalog_returns 
                      ON ( cr_item_sk = cs_item_sk 
                          AND cr_order_number = cs_order_number ) 
  WHERE inv_quantity_on_hand < cs_quantity 
        AND hd_buy_potential = '>10000' 
        AND d1.d_year = 1999 
        AND cd_marital_status = 'D' 
  GROUP  BY cs_item_sk, 
                cs_warehouse_sk, 
                d1.d_week_seq
)
SELECT TOP 100 i_item_desc, 
       w_warehouse_name, 
       d_week_seq,
       no_promo,
       promo,
       total_cnt
FROM top_items 
JOIN warehouse 
  ON ( w_warehouse_sk = cs_warehouse_sk ) 
JOIN item 
  ON ( i_item_sk = cs_item_sk ) 
ORDER  BY total_cnt DESC, 
          1, 2, 3;--

-- query73
SELECT c_last_name, 
       c_first_name, 
       c_salutation, 
       c_preferred_cust_flag, 
       ss_ticket_number, 
       cnt 
FROM   (SELECT ss_ticket_number, 
               ss_customer_sk, 
               Count(*) cnt 
        FROM   store_sales, 
               date_dim, 
               store, 
               household_demographics 
        WHERE  store_sales.ss_sold_date_sk = date_dim.d_date_sk 
               AND store_sales.ss_store_sk = store.s_store_sk 
               AND store_sales.ss_hdemo_sk = household_demographics.hd_demo_sk 
               AND date_dim.d_dom BETWEEN 1 AND 2 
               AND ( household_demographics.hd_buy_potential = '>10000' 
                      OR household_demographics.hd_buy_potential = 'Unknown' ) 
               AND household_demographics.hd_vehicle_count > 0 
               AND CASE 
                     WHEN household_demographics.hd_vehicle_count > 0 THEN 
                     household_demographics.hd_dep_count / 
                     household_demographics.hd_vehicle_count 
                     ELSE NULL 
                   END > 1 
               AND date_dim.d_year IN ( 1999, 1999 + 1, 1999 + 2 ) 
               AND store.s_county IN (  'Williamson County', 
                                       'Franklin Parish','Bronx County','Orange County' ) 
        GROUP  BY ss_ticket_number, 
                  ss_customer_sk) dj, 
       customer 
WHERE  ss_customer_sk = c_customer_sk 
       AND cnt BETWEEN 1 AND 5 
ORDER  BY cnt DESC, 
          c_last_name ASC;-- 

-- query74
WITH year_total 
     AS (SELECT c_customer_id    customer_id, 
                c_first_name     customer_first_name, 
                c_last_name      customer_last_name, 
                d_year           AS year1, 
                Sum(ss_net_paid) year_total, 
                's'              sale_type 
         FROM   customer, 
                store_sales, 
                date_dim 
         WHERE  c_customer_sk = ss_customer_sk 
                AND ss_sold_date_sk = d_date_sk 
                AND d_year IN ( 2001, 2001 + 1 ) 
         GROUP  BY c_customer_id, 
                   c_first_name, 
                   c_last_name, 
                   d_year 
         UNION ALL 
         SELECT c_customer_id    customer_id, 
                c_first_name     customer_first_name, 
                c_last_name      customer_last_name, 
                d_year           AS year1, 
                Sum(ws_net_paid) year_total, 
                'w'              sale_type 
         FROM   customer, 
                web_sales, 
                date_dim 
         WHERE  c_customer_sk = ws_bill_customer_sk 
                AND ws_sold_date_sk = d_date_sk 
                AND d_year IN ( 2001, 2001 + 1 ) 
         GROUP  BY c_customer_id, 
                   c_first_name, 
                   c_last_name, 
                   d_year) 
SELECT TOP 100 t_s_secyear.customer_id, 
               t_s_secyear.customer_first_name, 
               t_s_secyear.customer_last_name 
FROM   year_total t_s_firstyear, 
       year_total t_s_secyear, 
       year_total t_w_firstyear, 
       year_total t_w_secyear 
WHERE  t_s_secyear.customer_id = t_s_firstyear.customer_id 
       AND t_s_firstyear.customer_id = t_w_secyear.customer_id 
       AND t_s_firstyear.customer_id = t_w_firstyear.customer_id 
       AND t_s_firstyear.sale_type = 's' 
       AND t_w_firstyear.sale_type = 'w' 
       AND t_s_secyear.sale_type = 's' 
       AND t_w_secyear.sale_type = 'w' 
       AND t_s_firstyear.year1 = 2001 
       AND t_s_secyear.year1 = 2001 + 1 
       AND t_w_firstyear.year1 = 2001 
       AND t_w_secyear.year1 = 2001 + 1 
       AND t_s_firstyear.year_total > 0 
       AND t_w_firstyear.year_total > 0 
       AND CASE 
             WHEN t_w_firstyear.year_total > 0 THEN t_w_secyear.year_total / 
                                                    t_w_firstyear.year_total 
             ELSE NULL 
           END > CASE 
                   WHEN t_s_firstyear.year_total > 0 THEN 
                   t_s_secyear.year_total / 
                   t_s_firstyear.year_total 
                   ELSE NULL 
                 END 
ORDER  BY 1;--

-- query75
WITH all_sales 
     AS (SELECT d_year, 
                i_brand_id, 
                i_class_id, 
                i_category_id, 
                i_manufact_id, 
                Sum(sales_cnt) AS sales_cnt, 
                Sum(sales_amt) AS sales_amt 
         FROM   (SELECT d_year, 
                        i_brand_id, 
                        i_class_id, 
                        i_category_id, 
                        i_manufact_id, 
                        cs_quantity - COALESCE(cr_return_quantity, 0)        AS 
                        sales_cnt, 
                        cs_ext_sales_price - COALESCE(cr_return_amount, 0.0) AS 
                        sales_amt 
                 FROM   catalog_sales 
                        JOIN item 
                          ON i_item_sk = cs_item_sk 
                        JOIN date_dim 
                          ON d_date_sk = cs_sold_date_sk 
                        LEFT JOIN catalog_returns 
                               ON ( cs_order_number = cr_order_number 
                                    AND cs_item_sk = cr_item_sk ) 
                 WHERE  i_category = 'Books' 
                 UNION ALL
                 SELECT d_year, 
                        i_brand_id, 
                        i_class_id, 
                        i_category_id, 
                        i_manufact_id, 
                        ss_quantity - COALESCE(sr_return_quantity, 0)     AS 
                        sales_cnt, 
                        ss_ext_sales_price - COALESCE(sr_return_amt, 0.0) AS 
                        sales_amt 
                 FROM   store_sales 
                        JOIN item 
                          ON i_item_sk = ss_item_sk 
                        JOIN date_dim 
                          ON d_date_sk = ss_sold_date_sk 
                        LEFT JOIN store_returns 
                               ON ( ss_ticket_number = sr_ticket_number 
                                    AND ss_item_sk = sr_item_sk ) 
                 WHERE  i_category = 'Books' 
                 UNION ALL
                 SELECT d_year, 
                        i_brand_id, 
                        i_class_id, 
                        i_category_id, 
                        i_manufact_id, 
                        ws_quantity - COALESCE(wr_return_quantity, 0)     AS 
                        sales_cnt, 
                        ws_ext_sales_price - COALESCE(wr_return_amt, 0.0) AS 
                        sales_amt 
                 FROM   web_sales 
                        JOIN item 
                          ON i_item_sk = ws_item_sk 
                        JOIN date_dim 
                          ON d_date_sk = ws_sold_date_sk 
                        LEFT JOIN web_returns 
                               ON ( ws_order_number = wr_order_number 
                                    AND ws_item_sk = wr_item_sk ) 
                 WHERE  i_category = 'Books') sales_detail 
         GROUP  BY d_year, 
                   i_brand_id, 
                   i_class_id, 
                   i_category_id, 
                   i_manufact_id) 
SELECT TOP 100 prev_yr.d_year                        AS prev_year, 
               curr_yr.d_year                        AS year1, 
               curr_yr.i_brand_id, 
               curr_yr.i_class_id, 
               curr_yr.i_category_id, 
               curr_yr.i_manufact_id, 
               prev_yr.sales_cnt                     AS prev_yr_cnt, 
               curr_yr.sales_cnt                     AS curr_yr_cnt, 
               curr_yr.sales_cnt - prev_yr.sales_cnt AS sales_cnt_diff, 
               curr_yr.sales_amt - prev_yr.sales_amt AS sales_amt_diff 
FROM   all_sales curr_yr, 
       all_sales prev_yr 
WHERE  curr_yr.i_brand_id = prev_yr.i_brand_id 
       AND curr_yr.i_class_id = prev_yr.i_class_id 
       AND curr_yr.i_category_id = prev_yr.i_category_id 
       AND curr_yr.i_manufact_id = prev_yr.i_manufact_id 
       AND curr_yr.d_year = 2002 
       AND prev_yr.d_year = 2002 - 1 
       AND curr_yr.sales_cnt / prev_yr.sales_cnt
           < 0.9 
ORDER  BY sales_cnt_diff;--

-- query76
select top 100 channel, col_name, d_year, d_qoy, i_category, COUNT(*) sales_cnt, SUM(ext_sales_price) sales_amt FROM (
        SELECT 'store' as channel, 'ss_store_sk' col_name, d_year, d_qoy, i_category, ss_ext_sales_price ext_sales_price
         FROM store_sales, item, date_dim
         WHERE ss_store_sk IS NULL
           AND ss_sold_date_sk=d_date_sk
           AND ss_item_sk=i_item_sk
        UNION ALL
        SELECT 'web' as channel, 'ws_ship_customer_sk' col_name, d_year, d_qoy, i_category, ws_ext_sales_price ext_sales_price
         FROM web_sales, item, date_dim
         WHERE ws_ship_customer_sk IS NULL
           AND ws_sold_date_sk=d_date_sk
           AND ws_item_sk=i_item_sk
        UNION ALL
        SELECT 'catalog' as channel, 'cs_ship_addr_sk' col_name, d_year, d_qoy, i_category, cs_ext_sales_price ext_sales_price
         FROM catalog_sales, item, date_dim
         WHERE cs_ship_addr_sk IS NULL
           AND cs_sold_date_sk=d_date_sk
           AND cs_item_sk=i_item_sk) foo
GROUP BY channel, col_name, d_year, d_qoy, i_category
ORDER BY channel, col_name, d_year, d_qoy, i_category
;--
-- query77
with ss as
 (select s_store_sk,
         sum(ss_ext_sales_price) as sales,
         sum(ss_net_profit) as profit
 from store_sales,
      date_dim,
      store
 where ss_sold_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date) 
                  and (dateadd(day,30,cast('2000-08-23' as date))) 
       and ss_store_sk = s_store_sk
 group by s_store_sk)
 ,
 sr as
 (select s_store_sk,
         sum(sr_return_amt) as returns,
         sum(sr_net_loss) as profit_loss
 from store_returns,
      date_dim,
      store
 where sr_returned_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
       and sr_store_sk = s_store_sk
 group by s_store_sk), 
 cs as
 (select cs_call_center_sk,
        sum(cs_ext_sales_price) as sales,
        sum(cs_net_profit) as profit
 from catalog_sales,
      date_dim
 where cs_sold_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
 group by cs_call_center_sk 
 ), 
 cr as
 (select cr_call_center_sk,
         sum(cr_return_amount) as returns,
         sum(cr_net_loss) as profit_loss
 from catalog_returns,
      date_dim
 where cr_returned_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
 group by cr_call_center_sk
 ), 
 ws as
 ( select wp_web_page_sk,
        sum(ws_ext_sales_price) as sales,
        sum(ws_net_profit) as profit
 from web_sales,
      date_dim,
      web_page
 where ws_sold_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
       and ws_web_page_sk = wp_web_page_sk
 group by wp_web_page_sk), 
 wr as
 (select wp_web_page_sk,
        sum(wr_return_amt) as returns,
        sum(wr_net_loss) as profit_loss
 from web_returns,
      date_dim,
      web_page
 where wr_returned_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
       and wr_web_page_sk = wp_web_page_sk
 group by wp_web_page_sk)
  select top 100 channel
        , id
        , sum(sales) as sales
        , sum(returns) as returns
        , sum(profit) as profit
 from 
 (select 'store channel' as channel
        , ss.s_store_sk as id
        , sales
        , coalesce(returns, 0) as returns
        , (profit - coalesce(profit_loss,0)) as profit
 from   ss left join sr
        on  ss.s_store_sk = sr.s_store_sk
 union all
 select 'catalog channel' as channel
        , cs_call_center_sk as id
        , sales
        , returns
        , (profit - profit_loss) as profit
 from  cs
       , cr
 union all
 select 'web channel' as channel
        , ws.wp_web_page_sk as id
        , sales
        , coalesce(returns, 0) returns
        , (profit - coalesce(profit_loss,0)) as profit
 from   ws left join wr
        on  ws.wp_web_page_sk = wr.wp_web_page_sk
 ) x
 group by rollup (channel, id)
 order by channel
         ,id
 ;-- 

-- query78
with ws as
  (select d_year AS ws_sold_year, ws_item_sk,
    ws_bill_customer_sk ws_customer_sk,
    sum(ws_quantity) ws_qty,
    sum(ws_wholesale_cost) ws_wc,
    sum(ws_sales_price) ws_sp
   from web_sales
   left join web_returns on wr_order_number=ws_order_number and ws_item_sk=wr_item_sk
   join date_dim on ws_sold_date_sk = d_date_sk
   where wr_order_number is null
   group by d_year, ws_item_sk, ws_bill_customer_sk
   ),
cs as
  (select d_year AS cs_sold_year, cs_item_sk,
    cs_bill_customer_sk cs_customer_sk,
    sum(cs_quantity) cs_qty,
    sum(cs_wholesale_cost) cs_wc,
    sum(cs_sales_price) cs_sp
   from catalog_sales
   left join catalog_returns on cr_order_number=cs_order_number and cs_item_sk=cr_item_sk
   join date_dim on cs_sold_date_sk = d_date_sk
   where cr_order_number is null
   group by d_year, cs_item_sk, cs_bill_customer_sk
   ),
ss as
  (select d_year AS ss_sold_year, ss_item_sk,
    ss_customer_sk,
    sum(ss_quantity) ss_qty,
    sum(ss_wholesale_cost) ss_wc,
    sum(ss_sales_price) ss_sp
   from store_sales
   left join store_returns on sr_ticket_number=ss_ticket_number and ss_item_sk=sr_item_sk
   join date_dim on ss_sold_date_sk = d_date_sk
   where sr_ticket_number is null
   group by d_year, ss_item_sk, ss_customer_sk
   )
SELECT top 100
ss_sold_year, ss_item_sk, ss_customer_sk,
round(ss_qty/(coalesce(ws_qty,0)+coalesce(cs_qty,0)),2) ratio,
ss_qty store_qty, ss_wc store_wholesale_cost, ss_sp store_sales_price,
coalesce(ws_qty,0)+coalesce(cs_qty,0) other_chan_qty,
coalesce(ws_wc,0)+coalesce(cs_wc,0) other_chan_wholesale_cost,
coalesce(ws_sp,0)+coalesce(cs_sp,0) other_chan_sales_price
from ss
left join ws on (ws_sold_year=ss_sold_year and ws_item_sk=ss_item_sk and ws_customer_sk=ss_customer_sk)
left join cs on (cs_sold_year=ss_sold_year and cs_item_sk=ss_item_sk and cs_customer_sk=ss_customer_sk)
where (coalesce(ws_qty,0)>0 or coalesce(cs_qty, 0)>0) and ss_sold_year=2000
order by 
  ss_sold_year, ss_item_sk, ss_customer_sk,
  ss_qty desc, ss_wc desc, ss_sp desc,
  other_chan_qty,
  other_chan_wholesale_cost,
  other_chan_sales_price,
  ratio
  ;--
-- query79
SELECT TOP 100 c_last_name, 
               c_first_name, 
               Substring(s_city, 1, 30), 
               ss_ticket_number, 
               amt, 
               profit 
FROM   (SELECT ss_ticket_number, 
               ss_customer_sk, 
               store.s_city, 
               Sum(ss_coupon_amt) amt, 
               Sum(ss_net_profit) profit 
        FROM   store_sales, 
               date_dim, 
               store, 
               household_demographics 
        WHERE  store_sales.ss_sold_date_sk = date_dim.d_date_sk 
               AND store_sales.ss_store_sk = store.s_store_sk 
               AND store_sales.ss_hdemo_sk = household_demographics.hd_demo_sk 
               AND ( household_demographics.hd_dep_count = 6 
                      OR household_demographics.hd_vehicle_count > 2 ) 
               AND date_dim.d_dow = 1 
               AND date_dim.d_year IN ( 2000-1, 2000, 2000 + 1 ) 
               AND store.s_number_employees BETWEEN 200 AND 295 
        GROUP  BY ss_ticket_number, 
                  ss_customer_sk, 
                  ss_addr_sk, 
                  store.s_city) ms, 
       customer 
WHERE  ss_customer_sk = c_customer_sk 
ORDER  BY c_last_name, 
          c_first_name, 
          Substring(s_city, 1, 30), 
          profit
;--
-- query80
with ssr as
 (select  s_store_id as store_id,
          sum(ss_ext_sales_price) as sales,
          sum(coalesce(sr_return_amt, 0)) as returns,
          sum(ss_net_profit - coalesce(sr_net_loss, 0)) as profit
  from store_sales left outer join store_returns on
         (ss_item_sk = sr_item_sk and ss_ticket_number = sr_ticket_number),
     date_dim,
     store,
     item,
     promotion
 where ss_sold_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date) 
                  and (dateadd(day,30,cast('2000-08-23' as date)))
       and ss_store_sk = s_store_sk
       and ss_item_sk = i_item_sk
       and i_current_price > 50
       and ss_promo_sk = p_promo_sk
       and p_channel_tv = 'N'
 group by s_store_id)
 ,
 csr as
 (select  cp_catalog_page_id as catalog_page_id,
          sum(cs_ext_sales_price) as sales,
          sum(coalesce(cr_return_amount, 0)) as returns,
          sum(cs_net_profit - coalesce(cr_net_loss, 0)) as profit
  from catalog_sales left outer join catalog_returns on
         (cs_item_sk = cr_item_sk and cs_order_number = cr_order_number),
     date_dim,
     catalog_page,
     item,
     promotion
 where cs_sold_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
        and cs_catalog_page_sk = cp_catalog_page_sk
       and cs_item_sk = i_item_sk
       and i_current_price > 50
       and cs_promo_sk = p_promo_sk
       and p_channel_tv = 'N'
group by cp_catalog_page_id)
 ,
 wsr as
 (select  web_site_id,
          sum(ws_ext_sales_price) as sales,
          sum(coalesce(wr_return_amt, 0)) as returns,
          sum(ws_net_profit - coalesce(wr_net_loss, 0)) as profit
  from web_sales left outer join web_returns on
         (ws_item_sk = wr_item_sk and ws_order_number = wr_order_number),
     date_dim,
     web_site,
     item,
     promotion
 where ws_sold_date_sk = d_date_sk
       and d_date between cast('2000-08-23' as date)
                  and (dateadd(day,30,cast('2000-08-23' as date)))
        and ws_web_site_sk = web_site_sk
       and ws_item_sk = i_item_sk
       and i_current_price > 50
       and ws_promo_sk = p_promo_sk
       and p_channel_tv = 'N'
group by web_site_id)
  select top 100 channel
        , id
        , sum(sales) as sales
        , sum(returns) as returns
        , sum(profit) as profit
 from 
 (select 'store channel' as channel
        , 'store' + store_id as id
        , sales
        , returns
        , profit
 from   ssr
 union all
 select 'catalog channel' as channel
        , 'catalog_page' + catalog_page_id as id
        , sales
        , returns
        , profit
 from  csr
 union all
 select 'web channel' as channel
        , 'web_site' + web_site_id as id
        , sales
        , returns
        , profit
 from   wsr
 ) x
 group by rollup (channel, id)
 order by channel
         ,id
 ;--

-- query81
WITH customer_total_return 
     AS (SELECT cr_returning_customer_sk   AS ctr_customer_sk, 
                ca_state                   AS ctr_state, 
                Sum(cr_return_amt_inc_tax) AS ctr_total_return 
         FROM   catalog_returns, 
                date_dim, 
                customer_address 
         WHERE  cr_returned_date_sk = d_date_sk 
                AND d_year = 1999+1 
                AND cr_returning_addr_sk = ca_address_sk 
         GROUP  BY cr_returning_customer_sk, 
                   ca_state),
high_return AS (
    SELECT ctr_state AS hr_state, Avg(ctr_total_return) * 1.2 AS hr_limit
    FROM   customer_total_return 
    GROUP BY ctr_state
)
SELECT TOP 100 c_customer_id, 
               c_salutation, 
               c_first_name, 
               c_last_name, 
               ca_street_number, 
               ca_street_name, 
               ca_street_type, 
               ca_suite_number, 
               ca_city, 
               ca_county, 
               ca_state, 
               ca_zip, 
               ca_country, 
               ca_gmt_offset, 
               ca_location_type, 
               ctr_total_return 
FROM   customer_total_return, 
       high_return,
       customer_address, 
       customer
WHERE  ctr_state = hr_state 
       AND ctr_customer_sk = c_customer_sk 
       AND ca_address_sk = c_current_addr_sk 
       AND ca_state = 'GA' 
       AND ctr_total_return > hr_limit
ORDER  BY c_customer_id, 
          c_salutation, 
          c_first_name, 
          c_last_name, 
          ca_street_number, 
          ca_street_name, 
          ca_street_type, 
          ca_suite_number, 
          ca_city, 
          ca_county, 
          ca_state, 
          ca_zip, 
          ca_country, 
          ca_gmt_offset, 
          ca_location_type, 
          ctr_total_return;--

-- query82
select top 100 i_item_id
       ,i_item_desc
       ,i_current_price
 from item, inventory, date_dim, store_sales
 where i_current_price between 62 and 62+30
 and inv_item_sk = i_item_sk
 and d_date_sk=inv_date_sk
 and d_date between cast('2000-05-25' as date) and (dateadd(day,60,cast('2000-05-25' as date)))
 and i_manufact_id in (129,270,821,423)
 and inv_quantity_on_hand between 100 and 500
 and ss_item_sk = i_item_sk
 group by i_item_id,i_item_desc,i_current_price
 order by i_item_id
 ;--

-- query83
with sr_items as
 (select i_item_id item_id,
        sum(sr_return_quantity) sr_item_qty
 from store_returns,
      item,
      date_dim
 where sr_item_sk = i_item_sk
 and   d_date    in 
	(select d_date
	from date_dim
	where d_week_seq in 
		(select d_week_seq
		from date_dim
	  where d_date in ('2000-06-30','2000-09-27','2000-11-17')))
 and   sr_returned_date_sk   = d_date_sk
 group by i_item_id),
 cr_items as
 (select i_item_id item_id,
        sum(cr_return_quantity) cr_item_qty
 from catalog_returns,
      item,
      date_dim
 where cr_item_sk = i_item_sk
 and   d_date    in 
	(select d_date
	from date_dim
	where d_week_seq in 
		(select d_week_seq
		from date_dim
	  where d_date in ('2000-06-30','2000-09-27','2000-11-17')))
 and   cr_returned_date_sk   = d_date_sk
 group by i_item_id),
 wr_items as
 (select i_item_id item_id,
        sum(wr_return_quantity) wr_item_qty
 from web_returns,
      item,
      date_dim
 where wr_item_sk = i_item_sk
 and   d_date    in 
	(select d_date
	from date_dim
	where d_week_seq in 
		(select d_week_seq
		from date_dim
		where d_date in ('2000-06-30','2000-09-27','2000-11-17')))
 and   wr_returned_date_sk   = d_date_sk
 group by i_item_id)
 select top 100 sr_items.item_id
       ,sr_item_qty
       ,sr_item_qty/(sr_item_qty+cr_item_qty+wr_item_qty)/3.0 * 100 sr_dev
       ,cr_item_qty
       ,cr_item_qty/(sr_item_qty+cr_item_qty+wr_item_qty)/3.0 * 100 cr_dev
       ,wr_item_qty
       ,wr_item_qty/(sr_item_qty+cr_item_qty+wr_item_qty)/3.0 * 100 wr_dev
       ,(sr_item_qty+cr_item_qty+wr_item_qty)/3.0 average
 from sr_items
     ,cr_items
     ,wr_items
 where sr_items.item_id=cr_items.item_id
   and sr_items.item_id=wr_items.item_id 
 order by sr_items.item_id
         ,sr_item_qty
;--

-- query84
SELECT TOP 100 c_customer_id   AS customer_id, 
       Concat(c_last_name, Concat(', ', c_first_name))  AS customername 
FROM   customer, 
       customer_address, 
       customer_demographics, 
       household_demographics, 
       income_band, 
       store_returns 
WHERE  ca_city = 'Edgewood' 
       AND c_current_addr_sk = ca_address_sk 
       AND ib_lower_bound >= 38128 
       AND ib_upper_bound <= 38128 + 50000 
       AND ib_income_band_sk = hd_income_band_sk 
       AND cd_demo_sk = c_current_cdemo_sk 
       AND hd_demo_sk = c_current_hdemo_sk 
       AND sr_cdemo_sk = cd_demo_sk 
ORDER  BY c_customer_id;--

-- query85
SELECT TOP 100 Substring(r_reason_desc, 1, 20), 
               Avg(ws_quantity), 
               Avg(wr_refunded_cash), 
               Avg(wr_fee) 
FROM   web_sales, 
       web_returns, 
       web_page, 
       customer_demographics cd1, 
       customer_demographics cd2, 
       customer_address, 
       date_dim, 
       reason 
WHERE  ws_web_page_sk = wp_web_page_sk 
       AND ws_item_sk = wr_item_sk 
       AND ws_order_number = wr_order_number 
       AND ws_sold_date_sk = d_date_sk 
       AND d_year = 2000 
       AND cd1.cd_demo_sk = wr_refunded_cdemo_sk 
       AND cd2.cd_demo_sk = wr_returning_cdemo_sk 
       AND ca_address_sk = wr_refunded_addr_sk 
       AND r_reason_sk = wr_reason_sk 
       AND ( ( cd1.cd_marital_status = 'M' 
               AND cd1.cd_marital_status = cd2.cd_marital_status 
               AND cd1.cd_education_status = 'Advanced Degree' 
               AND cd1.cd_education_status = cd2.cd_education_status 
               AND ws_sales_price BETWEEN 100.00 AND 150.00 ) 
              OR ( cd1.cd_marital_status = 'S' 
                   AND cd1.cd_marital_status = cd2.cd_marital_status 
                   AND cd1.cd_education_status = 'College' 
                   AND cd1.cd_education_status = cd2.cd_education_status 
                   AND ws_sales_price BETWEEN 50.00 AND 100.00 ) 
              OR ( cd1.cd_marital_status = 'W' 
                   AND cd1.cd_marital_status = cd2.cd_marital_status 
                   AND cd1.cd_education_status = '2 yr Degree' 
                   AND cd1.cd_education_status = cd2.cd_education_status 
                   AND ws_sales_price BETWEEN 150.00 AND 200.00 ) ) 
       AND ( ( ca_country = 'United States' 
               AND ca_state IN ( 'IN', 'OH', 'NJ' ) 
               AND ws_net_profit BETWEEN 100 AND 200 ) 
              OR ( ca_country = 'United States' 
                   AND ca_state IN ( 'WI', 'CT', 'KY' ) 
                   AND ws_net_profit BETWEEN 150 AND 300 ) 
              OR ( ca_country = 'United States' 
                   AND ca_state IN ( 'LA', 'IA', 'AR' ) 
                   AND ws_net_profit BETWEEN 50 AND 250 ) ) 
GROUP  BY r_reason_desc 
ORDER  BY Substring(r_reason_desc, 1, 20), 
          Avg(ws_quantity), 
          Avg(wr_refunded_cash), 
          Avg(wr_fee);--

-- query86
select top 100  
    sum(ws_net_paid) as total_sum
   ,i_category
   ,i_class
   ,grouping(i_category)+grouping(i_class) as lochierarchy
   ,rank() over (
 	partition by grouping(i_category)+grouping(i_class),
 	case when grouping(i_class) = 0 then i_category end 
 	order by sum(ws_net_paid) desc) as rank_within_parent
 from
    web_sales
   ,date_dim       d1
   ,item
 where
    d1.d_month_seq between 1200 and 1200+11
 and d1.d_date_sk = ws_sold_date_sk
 and i_item_sk  = ws_item_sk
 group by rollup(i_category,i_class)
 order by
   lochierarchy desc,
   case when grouping(i_category)+grouping(i_class) = 0 then i_category end,
   rank_within_parent
 ;--
-- query87
select count(*) 
from ((select distinct c_last_name, c_first_name, d_date
       from store_sales, date_dim, customer
       where store_sales.ss_sold_date_sk = date_dim.d_date_sk
         and store_sales.ss_customer_sk = customer.c_customer_sk
         and d_month_seq between 1200 and 1200+11)
       except
      (select distinct c_last_name, c_first_name, d_date
       from catalog_sales, date_dim, customer
       where catalog_sales.cs_sold_date_sk = date_dim.d_date_sk
         and catalog_sales.cs_bill_customer_sk = customer.c_customer_sk
         and d_month_seq between 1200 and 1200+11)
       except
      (select distinct c_last_name, c_first_name, d_date
       from web_sales, date_dim, customer
       where web_sales.ws_sold_date_sk = date_dim.d_date_sk
         and web_sales.ws_bill_customer_sk = customer.c_customer_sk
         and d_month_seq between 1200 and 1200+11)
) cool_cust
;--



-- query88
select  *
from
 (select count(*) h8_30_to_9
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk   
     and ss_hdemo_sk = household_demographics.hd_demo_sk 
     and ss_store_sk = s_store_sk
     and time_dim.t_hour = 8
     and time_dim.t_minute >= 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2)) 
     and store.s_store_name = 'ese') s1,
 (select count(*) h9_to_9_30 
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk 
     and time_dim.t_hour = 9 
     and time_dim.t_minute < 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s2,
 (select count(*) h9_30_to_10 
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk
     and time_dim.t_hour = 9
     and time_dim.t_minute >= 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s3,
 (select count(*) h10_to_10_30
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk
     and time_dim.t_hour = 10 
     and time_dim.t_minute < 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s4,
 (select count(*) h10_30_to_11
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk
     and time_dim.t_hour = 10 
     and time_dim.t_minute >= 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s5,
 (select count(*) h11_to_11_30
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk 
     and time_dim.t_hour = 11
     and time_dim.t_minute < 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s6,
 (select count(*) h11_30_to_12
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk
     and time_dim.t_hour = 11
     and time_dim.t_minute >= 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s7,
 (select count(*) h12_to_12_30
 from store_sales, household_demographics , time_dim, store
 where ss_sold_time_sk = time_dim.t_time_sk
     and ss_hdemo_sk = household_demographics.hd_demo_sk
     and ss_store_sk = s_store_sk
     and time_dim.t_hour = 12
     and time_dim.t_minute < 30
     and ((household_demographics.hd_dep_count = 4 and household_demographics.hd_vehicle_count<=4+2) or
          (household_demographics.hd_dep_count = 2 and household_demographics.hd_vehicle_count<=2+2) or
          (household_demographics.hd_dep_count = 0 and household_demographics.hd_vehicle_count<=0+2))
     and store.s_store_name = 'ese') s8
;--


-- query89
SELECT TOP 100 * 
FROM  (SELECT i_category, 
              i_class, 
              i_brand, 
              s_store_name, 
              s_company_name, 
              d_moy, 
              Sum(ss_sales_price) sum_sales, 
              Avg(Sum(ss_sales_price)) 
                OVER ( 
                  partition BY i_category, i_brand, s_store_name, s_company_name 
                ) 
                                  avg_monthly_sales 
       FROM   item, 
              store_sales, 
              date_dim, 
              store 
       WHERE  ss_item_sk = i_item_sk 
              AND ss_sold_date_sk = d_date_sk 
              AND ss_store_sk = s_store_sk 
              AND d_year IN ( 1999 ) 
              AND ( ( i_category IN ( 'Books', 'Electronics', 'Sports' ) 
                      AND i_class IN ( 'computers', 'stereo', 'football' ) ) 
                     OR ( i_category IN ( 'Men', 'Jewelry', 'Women' ) 
                          AND i_class IN ( 'shirts', 'birdal', 'dresses' ) ) ) 
       GROUP  BY i_category, 
                 i_class, 
                 i_brand, 
                 s_store_name, 
                 s_company_name, 
                 d_moy) tmp1 
WHERE  CASE 
         WHEN ( avg_monthly_sales <> 0 ) THEN ( 
         Abs(sum_sales - avg_monthly_sales) / avg_monthly_sales ) 
         ELSE NULL 
       END > 0.1 
ORDER  BY sum_sales - avg_monthly_sales, 
          s_store_name;--

-- query90
 select top 100 cast(amc as decimal(15,4))/cast(pmc as decimal(15,4)) am_pm_ratio
 from ( select count(*) amc
       from web_sales, household_demographics , time_dim, web_page
       where ws_sold_time_sk = time_dim.t_time_sk
         and ws_ship_hdemo_sk = household_demographics.hd_demo_sk
         and ws_web_page_sk = web_page.wp_web_page_sk
         and time_dim.t_hour between 8 and 8+1
         and household_demographics.hd_dep_count = 6
         and web_page.wp_char_count between 5000 and 5200) at,
      ( select count(*) pmc
       from web_sales, household_demographics , time_dim, web_page
       where ws_sold_time_sk = time_dim.t_time_sk
         and ws_ship_hdemo_sk = household_demographics.hd_demo_sk
         and ws_web_page_sk = web_page.wp_web_page_sk
         and time_dim.t_hour between 19 and 19+1
         and household_demographics.hd_dep_count = 6
         and web_page.wp_char_count between 5000 and 5200) pt
 order by am_pm_ratio;--

-- query91
SELECT cc_call_center_id Call_Center, 
       cc_name           Call_Center_Name, 
       cc_manager        Manager, 
       Sum(cr_net_loss)  Returns_Loss 
FROM   call_center, 
       catalog_returns, 
       date_dim, 
       customer, 
       customer_address, 
       customer_demographics, 
       household_demographics 
WHERE  cr_call_center_sk = cc_call_center_sk 
       AND cr_returned_date_sk = d_date_sk 
       AND cr_returning_customer_sk = c_customer_sk 
       AND cd_demo_sk = c_current_cdemo_sk 
       AND hd_demo_sk = c_current_hdemo_sk 
       AND ca_address_sk = c_current_addr_sk 
       AND d_year = 1998 
       AND d_moy = 11
       AND ( ( cd_marital_status = 'M' 
               AND cd_education_status = 'Unknown' ) 
              OR ( cd_marital_status = 'W' 
                   AND cd_education_status = 'Advanced Degree' ) ) 
       AND hd_buy_potential LIKE 'Unknown' 
       AND ca_gmt_offset = -7 
GROUP  BY cc_call_center_id, 
          cc_name, 
          cc_manager, 
          cd_marital_status, 
          cd_education_status 
ORDER  BY Sum(cr_net_loss) DESC;-- 

-- query92
select top 100 
   sum(ws_ext_discount_amt)  as "Excess Discount Amount" 
from 
    web_sales 
   ,item 
   ,date_dim
where
i_manufact_id = 350 
and i_item_sk = ws_item_sk 
and d_date between '2000-01-27' and 
        (dateadd(day,90,cast('2000-01-27' as date)))
and d_date_sk = ws_sold_date_sk 
and ws_ext_discount_amt  
     > ( 
         SELECT 
            1.3 * avg(ws_ext_discount_amt) 
         FROM 
            web_sales 
           ,date_dim
         WHERE 
              ws_item_sk = i_item_sk 
          and d_date between '2000-01-27' and
                             (dateadd(day,90,cast('2000-01-27' as date)))
          and d_date_sk = ws_sold_date_sk 
      ) 
order by sum(ws_ext_discount_amt)
;--

-- query93
SELECT TOP 100 ss_customer_sk, 
               Sum(act_sales) sumsales 
FROM   (SELECT ss_item_sk, 
               ss_ticket_number, 
               ss_customer_sk, 
               CASE 
                 WHEN sr_return_quantity IS NOT NULL THEN 
                 ( ss_quantity - sr_return_quantity ) * ss_sales_price 
                 ELSE ( ss_quantity * ss_sales_price ) 
               END act_sales 
        FROM   store_sales 
               LEFT OUTER JOIN store_returns 
                            ON ( sr_item_sk = ss_item_sk 
                                 AND sr_ticket_number = ss_ticket_number ), 
               reason 
        WHERE  sr_reason_sk = r_reason_sk 
               AND r_reason_desc = 'reason 28') t 
GROUP  BY ss_customer_sk 
ORDER  BY sumsales, 
          ss_customer_sk;--

-- query94
select top 100
   count(distinct ws_order_number) as "order count"
  ,sum(ws_ext_ship_cost) as "total shipping cost"
  ,sum(ws_net_profit) as "total net profit"
from
   web_sales ws1
  ,date_dim
  ,customer_address
  ,web_site
where
    d_date between '1999-2-01' and 
           (dateadd(day,60,cast('1999-02-01' as date)))
and ws1.ws_ship_date_sk = d_date_sk
and ws1.ws_ship_addr_sk = ca_address_sk
and ca_state = 'IL'
and ws1.ws_web_site_sk = web_site_sk
and web_company_name = 'pri'
and exists (select *
            from web_sales ws2
            where ws1.ws_order_number = ws2.ws_order_number
              and ws1.ws_warehouse_sk <> ws2.ws_warehouse_sk)
and not exists(select *
               from web_returns wr1
               where ws1.ws_order_number = wr1.wr_order_number)
order by count(distinct ws_order_number)
;--
-- query95
with ws_wh as
(select ws1.ws_order_number,ws1.ws_warehouse_sk wh1,ws2.ws_warehouse_sk wh2
 from web_sales ws1,web_sales ws2
 where ws1.ws_order_number = ws2.ws_order_number
   and ws1.ws_warehouse_sk <> ws2.ws_warehouse_sk)
SELECT top 100 
   count(distinct ws_order_number) as "order count"
  ,sum(ws_ext_ship_cost) as "total shipping cost"
  ,sum(ws_net_profit) as "total net profit"
from
   web_sales ws1
  ,date_dim
  ,customer_address
  ,web_site
where
    d_date between '1999-2-01' and 
           (dateadd(day,60,cast('1999-02-01' as date)))
and ws1.ws_ship_date_sk = d_date_sk
and ws1.ws_ship_addr_sk = ca_address_sk
and ca_state = 'IL'
and ws1.ws_web_site_sk = web_site_sk
and web_company_name = 'pri'
and ws1.ws_order_number in (select ws_order_number
                            from ws_wh)
and ws1.ws_order_number in (select wr_order_number
                            from web_returns,ws_wh
                            where wr_order_number = ws_wh.ws_order_number)
order by count(distinct ws_order_number)
;--

-- query96
SELECT TOP 100 Count(*) 
FROM   store_sales, 
       household_demographics, 
       time_dim, 
       store 
WHERE  ss_sold_time_sk = time_dim.t_time_sk 
       AND ss_hdemo_sk = household_demographics.hd_demo_sk 
       AND ss_store_sk = s_store_sk 
       AND time_dim.t_hour = 20 
       AND time_dim.t_minute >= 30 
       AND household_demographics.hd_dep_count = 7 
       AND store.s_store_name = 'ese' 
ORDER  BY Count(*);--

-- query97
WITH ssci 
     AS (SELECT ss_customer_sk customer_sk, 
                ss_item_sk     item_sk 
         FROM   store_sales, 
                date_dim 
         WHERE  ss_sold_date_sk = d_date_sk 
                AND d_month_seq BETWEEN 1200 AND 1200 + 11 
         GROUP  BY ss_customer_sk, 
                   ss_item_sk), 
     csci 
     AS (SELECT cs_bill_customer_sk customer_sk, 
                cs_item_sk          item_sk 
         FROM   catalog_sales, 
                date_dim 
         WHERE  cs_sold_date_sk = d_date_sk 
                AND d_month_seq BETWEEN 1200 AND 1200 + 11 
         GROUP  BY cs_bill_customer_sk, 
                   cs_item_sk) 
SELECT TOP 100 Sum(CASE 
                     WHEN ssci.customer_sk IS NOT NULL 
                          AND csci.customer_sk IS NULL THEN 1 
                     ELSE 0 
                   END) store_only, 
               Sum(CASE 
                     WHEN ssci.customer_sk IS NULL 
                          AND csci.customer_sk IS NOT NULL THEN 1 
                     ELSE 0 
                   END) catalog_only, 
               Sum(CASE 
                     WHEN ssci.customer_sk IS NOT NULL 
                          AND csci.customer_sk IS NOT NULL THEN 1 
                     ELSE 0 
                   END) store_and_catalog 
FROM   ssci 
       FULL OUTER JOIN csci 
                    ON ( ssci.customer_sk = csci.customer_sk 
                         AND ssci.item_sk = csci.item_sk );--

-- query98
select i_item_id
      ,i_item_desc 
      ,i_category 
      ,i_class 
      ,i_current_price
      ,sum(ss_ext_sales_price) as itemrevenue 
      ,sum(ss_ext_sales_price)*100/sum(sum(ss_ext_sales_price)) over
          (partition by i_class) as revenueratio
from	
	store_sales
    	,item 
    	,date_dim
where 
	ss_item_sk = i_item_sk 
  	and i_category in ('Sports', 'Books', 'Home')
  	and ss_sold_date_sk = d_date_sk
	and d_date between cast('1999-02-22' as date) 
				and (dateadd(day,30,cast('1999-02-22' as date)))
group by 
	i_item_id
        ,i_item_desc 
        ,i_category
        ,i_class
        ,i_current_price
order by 
	i_category
        ,i_class
        ,i_item_id
        ,i_item_desc
        ,revenueratio;--


-- query99
SELECT TOP 100 Substring(w_warehouse_name, 1, 20), 
               sm_type, 
               cc_name, 
               Sum(CASE 
                     WHEN ( cs_ship_date_sk - cs_sold_date_sk <= 30 ) THEN 1 
                     ELSE 0 
                   END) AS days_30, 
               Sum(CASE 
                     WHEN ( cs_ship_date_sk - cs_sold_date_sk > 30 ) 
                          AND ( cs_ship_date_sk - cs_sold_date_sk <= 60 ) THEN 1 
                     ELSE 0 
                   END) AS days_31_60, 
               Sum(CASE 
                     WHEN ( cs_ship_date_sk - cs_sold_date_sk > 60 ) 
                          AND ( cs_ship_date_sk - cs_sold_date_sk <= 90 ) THEN 1 
                     ELSE 0 
                   END) AS days_61_90, 
               Sum(CASE 
                     WHEN ( cs_ship_date_sk - cs_sold_date_sk > 90 ) 
                          AND ( cs_ship_date_sk - cs_sold_date_sk <= 120 ) THEN 
                     1 
                     ELSE 0 
                   END) AS days_91_120, 
               Sum(CASE 
                     WHEN ( cs_ship_date_sk - cs_sold_date_sk > 120 ) THEN 1 
                     ELSE 0 
                   END) AS days_over_120 
FROM   catalog_sales, 
       warehouse, 
       ship_mode, 
       call_center, 
       date_dim 
WHERE  d_month_seq BETWEEN 1200 AND 1200 + 11 
       AND cs_ship_date_sk = d_date_sk 
       AND cs_warehouse_sk = w_warehouse_sk 
       AND cs_ship_mode_sk = sm_ship_mode_sk 
       AND cs_call_center_sk = cc_call_center_sk 
GROUP  BY Substring(w_warehouse_name, 1, 20), 
               sm_type, 
               cc_name
ORDER  BY 1, 2, 3
