USE TPCDS_RS;

ALTER TABLE customer_address ADD PRIMARY KEY(ca_address_sk)
ALTER TABLE customer_demographics ADD primary key (cd_demo_sk)
ALTER TABLE date_dim ADD primary key (d_date_sk)
ALTER TABLE warehouse ADD primary key (w_warehouse_sk)
ALTER TABLE ship_mode ADD primary key (sm_ship_mode_sk)
ALTER TABLE time_dim ADD primary key (t_time_sk)
ALTER TABLE reason ADD primary key (r_reason_sk)
ALTER TABLE income_band ADD primary key (ib_income_band_sk)
ALTER TABLE item ADD primary key (i_item_sk)
ALTER TABLE store ADD primary key (s_store_sk)
ALTER TABLE call_center ADD primary key (cc_call_center_sk)
ALTER TABLE customer ADD primary key (c_customer_sk)
ALTER TABLE web_site ADD primary key (web_site_sk)
ALTER TABLE store_returns ADD primary key (sr_item_sk, sr_ticket_number)
ALTER TABLE household_demographics ADD primary key (hd_demo_sk)
ALTER TABLE web_page ADD primary key (wp_web_page_sk)
ALTER TABLE promotion ADD primary key (p_promo_sk)
ALTER TABLE catalog_page ADD primary key (cp_catalog_page_sk)
ALTER TABLE inventory ADD primary key (inv_date_sk, inv_item_sk, inv_warehouse_sk)
ALTER TABLE catalog_returns ADD primary key (cr_item_sk, cr_order_number)
ALTER TABLE web_returns ADD primary key (wr_item_sk, wr_order_number)
ALTER TABLE web_sales ADD primary key (ws_item_sk, ws_order_number)
ALTER TABLE catalog_sales ADD primary key (cs_item_sk, cs_order_number)
ALTER TABLE store_sales ADD primary key (ss_item_sk, ss_ticket_number)








