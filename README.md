# 一个简单的orm工具

```java
    //....some code
    Query query = table.createQuery();
   
    //where name=? or name = ?
    query.where("name","张三").or("name","李四").list();

    //where name=? and (name=? or name=?)
    query.where("name","张三")
         .nest("name","李四").or("name","王五").end()
         .list();
    // where name like ?
    query.where().sql("name like ?","张三").list();    
```