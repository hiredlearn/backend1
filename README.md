# backend0
ERP Software


Here is the required domain info for the company ERP software:
- **Configurable products:** products with options based on an attribute, like color and/or size.
  - Eg. A product like a shirt in a Magento store is a configurable product, since you will be able to choose its size XL (example) and its color Blue.
    - When a customer adds one to their cart, in the software backend,  a simple product "related to a configurable" should also be added. 
    - There will there will be 2 items: 1 is a simple product (that has the attributes that you've chosen for the configurable product) and 1 configurable product (that is hidden from customer view).
    - The related simple shouldn't have a price value because it will only be in the related configurable.
    - Since the configurable stock is the sum of all its related simples, the ERP software should enter only the simple (for the inventory management), but the price for it must be taken from the configurable.
      - Each related simple has the parent_item_id equal to its corresponding configurable item_id.
  - On the other hand, **Independent simple** items dont have the parent_item_id .
