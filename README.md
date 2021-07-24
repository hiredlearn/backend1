# backend0
ERP Software


Here is the required domain info for the company ERP software:
  - Configurable products: products with options based on an attribute, like color and/or size.
    - Eg. A product like a shirt in a Magento store is a configurable product, since the customer will be able to choose its size/color (eg. XL, Blue).
  - When one is added to a customer's cart,  there will actually be 2 items added in the backend: 1 is a simple product (that has only the attributes that the customer chose) and 1 configurable product (that will only have the price value). 
  - Since the configurable stock is the sum of all its related simples, the ERP software should enter only the simple (for the inventory management), but the price may only be obtained from the configurable.
  - Each related simple has the parent_item_id equal to its corresponding configurable item_id while, Independent simple items do not have the parent_item_id .
