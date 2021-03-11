# 一个简单的orm工具

[![Maven Central](https://img.shields.io/maven-central/v/org.hswebframework/hsweb-easy-orm.svg?style=plastic)](http://search.maven.org/#search%7Cga%7C1%7Chsweb-easy-orm)
[![Build Status](https://travis-ci.com/hs-web/hsweb-easy-orm.svg?branch=master)](https://travis-ci.com/hs-web/hsweb-easy-orm)
[![codecov](https://codecov.io/gh/hs-web/hsweb-easy-orm/branch/master/graph/badge.svg)](https://codecov.io/gh/hs-web/hsweb-easy-orm)


# 场景

1. 轻SQL,重java.
2. 动态表单: 动态维护表结构,增删改查.
3. 参数驱动动态条件, 前端也能透传动态条件,无SQL注入.
4. 通用条件可拓展, 不再局限`=,>,like...`. `where("userId","user-in-org",orgId)//查询指定机构下用户的数据`
5. 真响应式支持, 封装r2dbc. reactor真香.

# 🌰

```java

DatabaseOperator operator = ...;
//DDL
operator.ddl()
        .createOrAlter("test_table")
        .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
        .addColumn().name("name").varchar(128).comment("名称").commit()
        .commit()
        .sync(); // reactive
     
//Query   
List<Map<String,Object>> dataList= operator.dml().query()
         .select("id")
         .from("test_table")
         .where(dsl->dsl.is("name","张三"))
         .fetch(mapList())
         .sync(); // reactive

```

# 使用

建议配合[hsweb4](https://github.com/hs-web/hsweb-framework/tree/4.0.x)使用.