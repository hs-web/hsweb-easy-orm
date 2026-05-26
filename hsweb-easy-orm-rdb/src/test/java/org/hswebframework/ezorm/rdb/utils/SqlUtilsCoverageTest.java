package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.PrepareSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.junit.Assert;
import org.junit.Test;
import org.slf4j.Logger;

import java.sql.JDBCType;

public class SqlUtilsCoverageTest {

    @Test
    public void testSqlParameterToStringBranches() {
        Assert.assertEquals("", SqlUtils.sqlParameterToString(null));
        Assert.assertEquals("1(Integer),null(null),null(String),foo(String)", SqlUtils.sqlParameterToString(new Object[]{1, null, NullValue.of(JdbcDataType.of(JDBCType.VARCHAR, String.class)), "foo"}));
    }

    @Test
    public void testPrintSqlBranches() {
        CapturingLogger disabled = new CapturingLogger(false);
        SqlUtils.printSql(disabled, PrepareSqlRequest.of("select 1", new Object[0]));
        Assert.assertFalse(disabled.called);

        CapturingLogger enabled = new CapturingLogger(true);
        SqlUtils.printSql(enabled, PrepareSqlRequest.of("select ?", new Object[]{1}));
        Assert.assertTrue(enabled.called);
        Assert.assertEquals("select * from t", SqlUtils.toNativeSql("select * from t", (Object[]) null));
    }

    @Test
    public void testReplaceSqlParameterCornerBranches() {
        Assert.assertEquals("1", SqlUtils.replaceSqlParameter("?", 0, i -> "1"));
        Assert.assertEquals("x 1 y", SqlUtils.replaceSqlParameter("x ? y", 0, i -> "1"));
        Assert.assertEquals("x ? 1", SqlUtils.replaceSqlParameter("x ? ?", 0, i -> "1"));
        Assert.assertEquals("x ?? y", SqlUtils.replaceSqlParameter("x ?? y", 0, i -> "1"));
        Assert.assertEquals("x ? array['a']", SqlUtils.replaceSqlParameter("x ? array['a']", 0, i -> "1"));
        Assert.assertEquals("x ? 'a'", SqlUtils.replaceSqlParameter("x ? 'a'", 0, i -> "1"));
        Assert.assertEquals("x ?| y", SqlUtils.replaceSqlParameter("x ?| y", 0, i -> "1"));
        Assert.assertEquals("x ?& y", SqlUtils.replaceSqlParameter("x ?& y", 0, i -> "1"));
        Assert.assertEquals("x ?! y", SqlUtils.replaceSqlParameter("x ?! y", 0, i -> "1"));
        Assert.assertEquals("x 1\n y", SqlUtils.replaceSqlParameter("x ?\n y", 0, i -> "1"));
        Assert.assertEquals("x /* ? */ y", SqlUtils.replaceSqlParameter("x /* ? */ y", 0, i -> "1"));
        Assert.assertEquals("x \"?\" y", SqlUtils.replaceSqlParameter("x \"?\" y", 0, i -> "1"));
    }

    @Test
    public void testCreateQuestionMarksBoundaryBranches() throws Exception {
        java.lang.reflect.Field field = SqlUtils.class.getDeclaredField("Q_M_CACHE");
        field.setAccessible(true);
        int cacheSize = ((Object[]) field.get(null)).length;
        Assert.assertTrue(SqlUtils.createQuestionMarks(0).isEmpty());
        Assert.assertEquals("?", SqlUtils.createQuestionMarks(1).toRequest().getSql());
        Assert.assertEquals("?,?", SqlUtils.createQuestionMarks(2).toRequest().getSql());
        Assert.assertEquals(1, SqlUtils.createQuestionMarks(1).getSql().size());
        Assert.assertTrue(SqlUtils.createQuestionMarks(cacheSize - 1).toRequest().getSql().contains("?"));
        Assert.assertTrue(SqlUtils.createQuestionMarks(cacheSize).toRequest().getSql().contains("?"));
        Assert.assertTrue(SqlUtils.createQuestionMarks(cacheSize + 1).toRequest().getSql().contains("?"));
    }

    private static class CapturingLogger implements Logger {
        private final boolean enabled;
        private boolean called;

        private CapturingLogger(boolean enabled) { this.enabled = enabled; }
        @Override public boolean isDebugEnabled() { return enabled; }
        @Override public void debug(String msg, Object... arguments) { called = true; }
        @Override public void debug(String msg) { called = true; }
        @Override public void debug(String format, Object arg) { called = true; }
        @Override public void debug(String format, Object arg1, Object arg2) { called = true; }
        @Override public void debug(String msg, Throwable t) { called = true; }
        @Override public boolean isTraceEnabled() { return false; }
        @Override public void trace(String msg) { }
        @Override public void trace(String format, Object arg) { }
        @Override public void trace(String format, Object arg1, Object arg2) { }
        @Override public void trace(String format, Object... arguments) { }
        @Override public void trace(String msg, Throwable t) { }
        @Override public boolean isTraceEnabled(org.slf4j.Marker marker) { return false; }
        @Override public void trace(org.slf4j.Marker marker, String msg) { }
        @Override public void trace(org.slf4j.Marker marker, String format, Object arg) { }
        @Override public void trace(org.slf4j.Marker marker, String format, Object arg1, Object arg2) { }
        @Override public void trace(org.slf4j.Marker marker, String format, Object... argArray) { }
        @Override public void trace(org.slf4j.Marker marker, String msg, Throwable t) { }
        @Override public boolean isDebugEnabled(org.slf4j.Marker marker) { return enabled; }
        @Override public void debug(org.slf4j.Marker marker, String msg) { called = true; }
        @Override public void debug(org.slf4j.Marker marker, String format, Object arg) { called = true; }
        @Override public void debug(org.slf4j.Marker marker, String format, Object arg1, Object arg2) { called = true; }
        @Override public void debug(org.slf4j.Marker marker, String format, Object... arguments) { called = true; }
        @Override public void debug(org.slf4j.Marker marker, String msg, Throwable t) { called = true; }
        @Override public boolean isInfoEnabled() { return false; }
        @Override public void info(String msg) { }
        @Override public void info(String format, Object arg) { }
        @Override public void info(String format, Object arg1, Object arg2) { }
        @Override public void info(String format, Object... arguments) { }
        @Override public void info(String msg, Throwable t) { }
        @Override public boolean isInfoEnabled(org.slf4j.Marker marker) { return false; }
        @Override public void info(org.slf4j.Marker marker, String msg) { }
        @Override public void info(org.slf4j.Marker marker, String format, Object arg) { }
        @Override public void info(org.slf4j.Marker marker, String format, Object arg1, Object arg2) { }
        @Override public void info(org.slf4j.Marker marker, String format, Object... arguments) { }
        @Override public void info(org.slf4j.Marker marker, String msg, Throwable t) { }
        @Override public boolean isWarnEnabled() { return false; }
        @Override public void warn(String msg) { }
        @Override public void warn(String format, Object arg) { }
        @Override public void warn(String format, Object... arguments) { }
        @Override public void warn(String format, Object arg1, Object arg2) { }
        @Override public void warn(String msg, Throwable t) { }
        @Override public boolean isWarnEnabled(org.slf4j.Marker marker) { return false; }
        @Override public void warn(org.slf4j.Marker marker, String msg) { }
        @Override public void warn(org.slf4j.Marker marker, String format, Object arg) { }
        @Override public void warn(org.slf4j.Marker marker, String format, Object arg1, Object arg2) { }
        @Override public void warn(org.slf4j.Marker marker, String format, Object... arguments) { }
        @Override public void warn(org.slf4j.Marker marker, String msg, Throwable t) { }
        @Override public boolean isErrorEnabled() { return false; }
        @Override public void error(String msg) { }
        @Override public void error(String format, Object arg) { }
        @Override public void error(String format, Object arg1, Object arg2) { }
        @Override public void error(String format, Object... arguments) { }
        @Override public void error(String msg, Throwable t) { }
        @Override public boolean isErrorEnabled(org.slf4j.Marker marker) { return false; }
        @Override public void error(org.slf4j.Marker marker, String msg) { }
        @Override public void error(org.slf4j.Marker marker, String format, Object arg) { }
        @Override public void error(org.slf4j.Marker marker, String format, Object arg1, Object arg2) { }
        @Override public void error(org.slf4j.Marker marker, String format, Object... arguments) { }
        @Override public void error(org.slf4j.Marker marker, String msg, Throwable t) { }
        @Override public String getName() { return "test"; }
    }
}
