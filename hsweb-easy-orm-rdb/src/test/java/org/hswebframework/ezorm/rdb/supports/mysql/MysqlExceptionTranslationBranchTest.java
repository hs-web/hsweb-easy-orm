package org.hswebframework.ezorm.rdb.supports.mysql;

import io.r2dbc.spi.R2dbcException;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.junit.Assert;
import org.junit.Test;

import java.sql.SQLException;

public class MysqlExceptionTranslationBranchTest {

    @Test
    public void testJdbcAndR2dbcBranches() {
        MysqlJDBCExceptionTranslation jdbc = MysqlJDBCExceptionTranslation.of((RDBSchemaMetadata) null);
        MysqlR2DBCExceptionTranslation r2dbc = MysqlR2DBCExceptionTranslation.of((RDBSchemaMetadata) null);

        RuntimeException plain = new RuntimeException("plain");
        Assert.assertSame(plain, jdbc.translate(plain));
        Assert.assertSame(plain, r2dbc.translate(plain));

        try {
            jdbc.translate(new SQLException("dup", "23000", 1062));
            Assert.fail();
        } catch (Throwable err) {
            Assert.assertTrue(err instanceof org.hswebframework.ezorm.rdb.exception.DuplicateKeyException);
        }

        try {
            r2dbc.translate(new TestR2dbcException(1022));
            Assert.fail();
        } catch (Throwable err) {
            Assert.assertTrue(err instanceof org.hswebframework.ezorm.rdb.exception.DuplicateKeyException);
        }
    }

    static class TestR2dbcException extends R2dbcException {
        TestR2dbcException(int code) {
            super("dup", "23000", code);
        }
    }
}
