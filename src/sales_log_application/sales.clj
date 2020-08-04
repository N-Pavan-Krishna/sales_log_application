(ns sales-log-application.sales)

"I am using atom data type to store the data of customer,
 products, and sales, as it is mutable. The Structure
 of the atom is Hash-Map, Key is table-name and value is
 another Hash Map containing entries with data as key and empty
 vector as value && indexes as key and empty Hash Map as value
 (containing id as key and data-id as value, to retrieve data
 quickly"

(def memory-db (atom {}))

"Used to get the database of this clojure application"
(defn read-db
  "Input  : empty
   Output : Data Structure of atom"
  []
  @memory-db)

"Used to modify the database of this clojure application"
(defn write-db
  "Input : Data Structure of atom
   Side effect : Overwrite the atom with new atom structure.
   Output : Data Structure of atom"
  [new-db]
  (reset! memory-db new-db))

"Used to create a entry for table in the database"
(defn create-table
  "Input  : name of table
   Side effect : Update the data structure of the atom
   Output : Data Structure of atom"
  [table-name]
  (let [new-db (assoc (read-db)
                 (keyword table-name) {
                                       :data [],
                                       :indexes {}
                                       })]
    (write-db new-db)
    )
  )

"Used to create sales table with customer and product
 details instead of their id's"
(defn get-name
  "Input : name of table, index-id of the record in the table
   Output : data record corresponding to the index-id
  "
  [table_name id]
  (let [record (get (read-db) (keyword table_name))
        data (get record :data)
        indexes (get record :indexes)
        index_in_data (get indexes (keyword (str id)))
        data_record (get data index_in_data)
        ]
    (get data_record :name)
    )
  )

"Used to populate the tables of database with corresponding
 records"
(defn insert
  "Input : name of table, record to insert
   Side effect : Updates the Structure of atom
   Output : Data Structure of the atom
  "
  [table-name, record]
  (if (= table-name "customer")
    (let [id (get record 0)
          name (get record 1)
          address (get record 2)
          phone-number (get record 3)
          ]
      (let [data (get (get (read-db) (keyword table-name)) :data )]
        (let [new-data (conj data {:id id, :name name, :address address, :phone-number phone-number}) ]
          (let [new-db-data (assoc-in (read-db) [(keyword table-name) :data] new-data)]
            (let [indexes (get (get (read-db) (keyword table-name)) :indexes )]
              (let [new-indexes  (conj indexes {(keyword id) (dec (count new-data))})]
                (let [new-indexes-data (assoc-in new-db-data [(keyword table-name) :indexes] new-indexes)]
                  (write-db new-indexes-data)
                  )
                )
              )
            )


          )
        )
      )
    (if (= table-name "product")
      (let [id (get record 0)
            name (get record 1)
            price (get record 2)
            ]
        (let [data (get (get (read-db) (keyword table-name)) :data )]
          (let [new-data (conj data {:id id, :name name, :price price}) ]
            (let [new-db-data (assoc-in (read-db) [(keyword table-name) :data] new-data)]
              (let [indexes (get (get (read-db) (keyword table-name)) :indexes )]
                (let [new-indexes  (conj indexes {(keyword id) (dec (count new-data))})]
                  (let [new-indexes-data (assoc-in new-db-data [(keyword table-name) :indexes] new-indexes)]
                    (write-db new-indexes-data)
                    )
                  )
                )
              )


            )
          )
        )
      (if (= table-name "sales")
        (let [id (get record 0)
              customer-name (get-name "customer" (get record 1))
              product-name (get-name "product" (get record 2))
              quantity (get record 3)
              ]
          (let [data (get (get (read-db) (keyword table-name)) :data )]
            (let [new-data (conj data {:id id, :customer-name customer-name, :product-name product-name, :quantity quantity}) ]
              (let [new-db-data (assoc-in (read-db) [(keyword table-name) :data] new-data)]
                (let [indexes (get (get (read-db) (keyword table-name)) :indexes )]
                  (let [new-indexes  (conj indexes {(keyword id) (dec (count new-data))})]
                    (let [new-indexes-data (assoc-in new-db-data [(keyword table-name) :indexes] new-indexes)]
                      (write-db new-indexes-data)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    )

  )

"Creating entries for the tables of customer, product,
 sales in the database"
(create-table "customer")
(create-table "product")
(create-table "sales")

"Reading tables from the .txt sales_log_application.files of customer, product,
 sales"
(def customer
  (clojure.string/split-lines (slurp "files/cust.txt"))
  )

(def product
  (clojure.string/split-lines (slurp "files/prod.txt"))
  )

(def sales
  (clojure.string/split-lines (slurp "files/sales.txt"))
  )

"Used to populate the database with data from customer,
 product and sales .txt file"
(defn load-database
  "Input : name of table, data of table, record number
   Side effect : Update the data structure of the atom
   Output : nil
  "
  [table-name table-data index]
  (if (< index (count table-data))
    (let [temp (clojure.string/split (get table-data index) #"\|") ]
      (type temp)
      (insert table-name temp)
      (load-database table-name table-data (+ index 1))
      )
    )
  )

"Loading the customer, product, sales data into atom"
(load-database "customer" customer 0)
(load-database "product" product 0)
(load-database "sales" sales 0)

"Used to print the records of tables of the database"
(defn display-data
  "Input : name of table, indexes data, record data, record number"
  [table_name indexes data index]
  (if (and (get indexes (keyword (str index))) (<= index (count data))  )
    (let [id (get indexes (keyword (str index)))]
      ;(println id)
      (let [output (clojure.string/replace (apply str (get data id)) "[" "") ]
        ;(println output)
        (println (clojure.string/replace output "]" "|"))
        )
      (display-data table_name indexes data (+ index 1))
      )
    (if (<= (+ index 1) (count data))
      (display-data table_name indexes data (+ index 1))
      )
    )
  )

"Used to print table name and call the display-data function"
(defn display
  "Input : name of the table"
  [table-name]
  (println (clojure.string/upper-case table-name))
  (let [records (get (read-db) (keyword table-name))
        data (get records :data)
        indexes (get records :indexes)
        ]
    ;(println "indexes :" indexes)
    (display-data table-name indexes data -1)
    )
  )


"Used to get the price of the product"
(defn get-price
  "Input : name of product
   Output : price of product"
  [product_name]
  (let [product (get (read-db) :product)
        data (get product :data)
        matched_data (filter #(= (:name %) product_name) data)
        ]
    ;(println matched_data)
    (get (nth matched_data 0) :price)
    )
  )

"Used to get the total sales price of the product"
(defn calculate-total-value
  "Input : data of product
   Output : total sales price of the product"
  [matched_data index total-price]
  (if (< index (count matched_data))
    (let [record (nth matched_data index)
          product_name (get record :product-name)
          price (Float/parseFloat (get-price product_name) )
          quantity (Float/parseFloat (get record :quantity))
          ]


      ;(println price)
      ;(println (type price))
      ;(println (type quantity))
      (calculate-total-value matched_data (+ index 1) (+ total-price (* price quantity)))
      ;(+ (* price quantity) (Float/parseFloat (calculate-total-value matched_data (+ index 1))) )
      )
    (* total-price 1)
    )
  )

"Used to display price of total purchases made by customer"
(defn sale-for-customer
  "Input : name of customer"
  [customer_name]
  (let [sales (get (read-db) :sales)
        data (get sales :data)
        matched_data (filter #(= (:customer-name %) customer_name) data)
        ]
    ;(println matched_data)
    (if (empty? matched_data)
      (do
        (let [customer (get (read-db) :customer)
              customer_data (get customer :data)
              customer_matched (filter #(= (:name %) customer_name) customer_data)
              ]
          ;(println customer_data)
          ;(println customer_matched)
          (if (empty? customer_matched)
            (println customer_name "Customer Not Found")
            (println customer_name "Customer didn't made any sales yet")
            )
          )

        )

      (do
        (print customer_name)
        (println (str " : $" (calculate-total-value matched_data 0 0)) )
        )
      )
    ;(println matched_data)

    )
  )


"Used to count the quantity of product that are sold out"
(defn calculate-quantity
  "Input : data of product, record index of product, total_quantity so far
   Output : total quantity of product sold"
  [matched-data index total-quantity]
  (if (< index (count matched-data))
    (let [record (nth matched-data index)
          quantity (Integer/parseInt (get record :quantity))
          ]
      (calculate-quantity matched-data (+ index 1) (+ total-quantity quantity))
      )
    (+ total-quantity 0)

    )
  )

"Used to display the quantity of product that are sold out"
(defn product-sold
  "Input : name of product"
  [product_name]
  (let [sales (get (read-db) :sales)
        data (get sales :data)
        matched_data (filter #(= (:product-name %) product_name) data)
        ]
    (if (empty? matched_data)
      (do
        (let [product (get (read-db) :product)
              product_data (get product :data)
              product_matched (filter #(= (:name %) product_name) product_data)
              ]
          ;(println customer_data)
          ;(println customer_matched)
          (if (empty? product_matched)
            (println product_name "Product Not Found")
            (println  "Zero Sales for the" product_name)
            )
          )
        )
      (do
        (print product_name)
        (println (str " : " (calculate-quantity matched_data 0 0)) )
        )
      )

    )
  )

"Used to display menu for the application recursively"
(defn run-sales-menu
  "Input : empty"
  []
  (println "*** Sales Menu ***")
  (println "-------------------")
  (println "1. Display Customer Table")
  (println "2. Display Product Table")
  (println "3. Display Sales Table")
  (println "4. Total Sales for Customer")
  (println "5. Total Count for Product")
  (println "6. Exit\n")

  (println "Enter an option?")


  (def input (read-line))
  (when (= input "1")
    (display "customer")
    )
  (when (= input "2")
    (display "product")
    )
  (when (= input "3")
    (display "sales")
    )
  (when (= input "4")
    (println "Enter Customer Name : ")
    (sale-for-customer (read-line))
    )
  (when (= input "5")
    (println "Enter Product Name : ")
    (product-sold (read-line))
    )
  (when (= input "6")
    (println "Good Bye")
    (System/exit 0)
    )
  (if (not (some #{input} '("1" "2" "3" "4" "5" "6")))
    (println "Please Enter Valid Option")
    )
  (recur)
  )

"Calling the application"
(run-sales-menu)

