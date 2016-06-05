# 一个简单的orm工具
#### 数据库支持:oracle,mysql,h2
#### 支持验证器,触发器 等可拓展功能 
## 快速上手
#### 核心

```java 
    Database database=...//如何创建,看下面
    //获取表
    Table table= database.getTable("s_user");
    //查询
    List data=table.createQuery().select("*").where("username","admin").list();//分页 .list(pageIndex,pageSize);
    //插入
    table.createInsert().value(data).exec();//批量插入调用 .values
    //更新
    table.createUpdate().set(data).where("id",id).exec();
    //删除
    table.createDelete().where("id",id).exec();
```

#### 我已经有表了!

```java
    SqlExecutor sqlExecutor = ....;//可继承AbstractJdbcSqlExecutor
    DatabaseMetaData databaseMetaData = new OracleDatabaseMeta();//oracle
    SimpleDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
    database.setTableMetaParser(new OracleTableMetaParser(sqlExecutor, "oracle_user_name"));//需要指定oracle的用户名
    Table user = database.getTable("s_user");//获取表,将自动解析数据库里的表s_user的结构,如果不存在或解析错误,将返回null
```

#### 我要新建表

```java
     SqlExecutor sqlExecutor = ....;//可继承AbstractJdbcSqlExecutor
     DatabaseMetaData databaseMetaData = new OracleDatabaseMeta();//oracle
     SimpleDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
     //创建表结构.在实际应用中,可通过json,xml,bean等创建
     TableMetaData metaData = new TableMetaData();
     metaData.setName("s_user");
     metaData.setAlias("user");
     FieldMetaData userName = new FieldMetaData();
     userName.setName("user_name");
     userName.setAlias("userName");
     userName.setJavaType(String.class);
     userName.setJdbcType(JDBCType.VARCHAR);
     userName.setDataType("varchar2(64)");
     metaData.addField(userName);
     //创建表
     database.createTable(metaData);
     //如果表已经存在
     //metaData.putTable(metaData);
```

#### 使用验证器
```java
     DatabaseMetaData databaseMetaData = new OracleDatabaseMeta();//oracle
     //在创建数据库定义对象的时候,自定义验证器工场类
     databaseMetaData.setValidatorFactory(yourFactory);
```

#### 使用自定义返回查询结果包装器
```java
     DatabaseMetaData databaseMetaData = new OracleDatabaseMeta();//oracle
     //在创建数据库定义对象的时候,自定义包装器工场类.不设置默认使用og.hsweb.ezorm.run.simple.wrapper.AdvancedMapWrapper 返回map结果
     databaseMetaData.setObjectWrapperFactory(yourFactory);
```