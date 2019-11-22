BULK INSERT store_returns
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\store_returns.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT call_center
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\call_center.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT catalog_returns
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\catalog_returns.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT catalog_sales
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\catalog_sales.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT customer
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\customer.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT customer_address
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\customer_address.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT customer_demographics
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\customer_demographics.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT date_dim
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\date_dim.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT dbgen_version
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\dbgen_version.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT ship_mode
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\ship_mode.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT household_demographics
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\household_demographics.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )


BULK INSERT income_band
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\income_band.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT inventory
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\inventory.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT item
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\item.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT promotion
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\promotion.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT reason
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\reason.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT store
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\store.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT store_returns
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\store_returns.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT store_sales
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\store_sales.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT web_page
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\web_page.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT web_returns
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\web_returns.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT web_sales
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\web_sales.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )
BULK INSERT web_site
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\web_site.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT time_dim
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\time_dim.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT warehouse
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\warehouse.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )

BULK INSERT catalog_page
    FROM 'C:\Users\Alex\Desktop\produced_dataset_1gb\produced_dataset\1gb\catalog_page.dat'
    WITH
    (
    FIRSTROW = 1,
    FIELDTERMINATOR = '|',  --CSV field delimiter
    ROWTERMINATOR = '0x0a',   --Use to shift the control to next row
    --ERRORFILE = 'C:\Users\Alex\Desktop\trash.dat',
    TABLOCK
    )