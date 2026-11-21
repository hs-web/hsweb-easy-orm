package org.hswebframework.ezorm.rdb.supports.kingbaseES.mssql;

import io.r2dbc.spi.R2dbcDataIntegrityViolationException;
import org.hswebframework.ezorm.rdb.exception.DuplicateKeyException;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;
import org.junit.Assert;
import org.junit.Test;

import java.sql.SQLException;

public class KingbaseSqlServerExceptionTranslationTest {

    private final ExceptionTranslation translation = new KingbaseSqlServerSchemaMetadata("dbo")
        .findFeatureNow(ExceptionTranslation.ID);

    @Test
    public void shouldTranslateJdbcDuplicateKeysAndKeepCause() {
        for (SQLException source : new SQLException[]{new SQLException("重复键", "23505"),
            new SQLException("duplicate", "23000", 2601), new SQLException("duplicate", "23000", 2627)}) {
            Throwable result = translation.translate(source);
            Assert.assertTrue(result instanceof DuplicateKeyException);
            Assert.assertSame(source, result.getCause());
        }
    }

    @Test
    public void shouldTranslateR2dbcDuplicateKeysAndKeepCause() {
        for (R2dbcDataIntegrityViolationException source : new R2dbcDataIntegrityViolationException[]{
            new R2dbcDataIntegrityViolationException("重复键", "23505", 0),
            new R2dbcDataIntegrityViolationException("duplicate", "23000", 2601),
            new R2dbcDataIntegrityViolationException("duplicate", "23000", 2627)}) {
            Throwable result = translation.translate(source);
            Assert.assertTrue(result instanceof DuplicateKeyException);
            Assert.assertSame(source, result.getCause());
        }
    }

    @Test
    public void shouldLeaveOtherConstraintAndSyntaxErrorsUnchanged() {
        for (Throwable source : new Throwable[]{new SQLException("not null", "23502"),
            new SQLException("foreign key", "23503"), new SQLException("duplicate key in SQL text", "42601"),
            new R2dbcDataIntegrityViolationException("not null", "23502", 0),
            new IllegalStateException("failure")}) {
            Assert.assertSame(source, translation.translate(source));
        }
    }

    @Test
    public void shouldTranslateDuplicateKeysWrappedByTheSynchronousBridge() {
        Throwable source = new R2dbcDataIntegrityViolationException("重复键", "23505", 0);
        Throwable result = translation.translate(new java.util.concurrent.ExecutionException(source));
        Assert.assertTrue(result instanceof DuplicateKeyException);
        Assert.assertSame(source, result.getCause());
        Throwable unrelated = new java.util.concurrent.ExecutionException(new IllegalStateException("failure"));
        Assert.assertSame(unrelated, translation.translate(unrelated));
    }
}
