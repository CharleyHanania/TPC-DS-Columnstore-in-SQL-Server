USE TPCDS_CS;

INSERT INTO TPCDS_RS.DBO.dbgen_version
SELECT * FROM dbgen_version

INSERT INTO TPCDS_RS.DBO.customer_address
SELECT * FROM customer_address

INSERT INTO TPCDS_RS.DBO.customer_demographics
SELECT * FROM customer_demographics

INSERT INTO TPCDS_RS.DBO.date_dim
SELECT * FROM date_dim

INSERT INTO TPCDS_RS.DBO.warehouse
SELECT * FROM warehouse

INSERT INTO TPCDS_RS.DBO.ship_mode
SELECT * FROM ship_mode

INSERT INTO TPCDS_RS.DBO.time_dim
SELECT * FROM time_dim

INSERT INTO TPCDS_RS.DBO.reason
SELECT * FROM reason

INSERT INTO TPCDS_RS.DBO.income_band
SELECT * FROM income_band

INSERT INTO TPCDS_RS.DBO.item
SELECT * FROM item

INSERT INTO TPCDS_RS.DBO.store
SELECT * FROM store

INSERT INTO TPCDS_RS.DBO.call_center
SELECT * FROM call_center

INSERT INTO TPCDS_RS.DBO.customer
SELECT * FROM customer

INSERT INTO TPCDS_RS.DBO.web_site
SELECT * FROM web_site

INSERT INTO TPCDS_RS.DBO.store_returns
SELECT * FROM store_returns

INSERT INTO TPCDS_RS.DBO.household_demographics
SELECT * FROM household_demographics

INSERT INTO TPCDS_RS.DBO.web_page
SELECT * FROM web_page

INSERT INTO TPCDS_RS.DBO.promotion
SELECT * FROM promotion

INSERT INTO TPCDS_RS.DBO.catalog_page
SELECT * FROM catalog_page

INSERT INTO TPCDS_RS.DBO.inventory
SELECT * FROM inventory

INSERT INTO TPCDS_RS.DBO.catalog_returns
SELECT * FROM catalog_returns

INSERT INTO TPCDS_RS.DBO.web_returns
SELECT * FROM web_returns

INSERT INTO TPCDS_RS.DBO.web_sales
SELECT * FROM web_sales

INSERT INTO TPCDS_RS.DBO.catalog_sales
SELECT * FROM catalog_sales

INSERT INTO TPCDS_RS.DBO.store_sales
SELECT * FROM store_sales