package org.hswebframework.ezorm.rdb.metadata.dialect;

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;

public class DialectTest {

    @Test
    public void test() {
        Dialect dialect = Dialect.H2;

        Assert.assertEquals(dialect.quote("name"), "\"NAME\"");

        Assert.assertEquals(dialect.clearQuote("\"NAME\""), "NAME");

        Assert.assertEquals(dialect.clearQuote("test.\"NAME\""), "test.NAME");

        Assert.assertEquals(dialect.clearQuote("db1.test.\"NAME\""), "db1.test.NAME");

    }

}