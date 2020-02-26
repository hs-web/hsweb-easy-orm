# 一个简单的orm工具

[![Maven Central](https://img.shields.io/maven-central/v/org.hswebframework/hsweb-easy-orm.svg?style=plastic)](http://search.maven.org/#search%7Cga%7C1%7Chsweb-easy-orm)
[![Build Status](https://travis-ci.com/hs-web/hsweb-easy-orm.svg?branch=4.0.x)](https://travis-ci.org/hs-web/hsweb-easy-orm)
[![codecov](https://codecov.io/gh/hs-web/hsweb-easy-orm/branch/4.0.x/graph/badge.svg)](https://codecov.io/gh/hs-web/hsweb-easy-orm)


# 🌰

```java

DatabaseOperator operator = ...;
//DDL
operator.ddl()
        .createOrAlter("test_table")
        .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
        .addColumn().name("name").varchar(128).comment("名称").commit()
        .commit()
        .sync(); //async or reactive
     
//Query   
List<Map<String,Object>> dataList= operator.dml().query()
         .select("id")
         .from("test_table")
         .where(dsl->dsl.is("name","张三"))
         .fetch(mapList())
         .sync(); //async or reactive

```