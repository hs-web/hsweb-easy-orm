package org.hswebframework.ezorm.rdb.utils;

import org.junit.Assert;
import org.junit.Test;

public class SqlUtilsBranchTest {

    @Test
    public void testReplaceSqlParameterKeepsQuotedStringsCommentsAndPgOperators() {
        String sql = "select '?', \"?\", col ? 'k', col ?| array['a'], col ?& array['b'], col ?! 'c', col = ? -- comment ?\n/* block ? */ and v = ?";
        String result = SqlUtils.replaceSqlParameter(sql, 0, i -> i == 0 ? "1" : "2");
        Assert.assertEquals("select '?', \"?\", col ? 'k', col ?| array['a'], col ?& array['b'], col ?! 'c', col = 1 -- comment ?\n/* block ? */ and v = 2", result);
    }

    @Test
    public void testReplaceSqlParameterDetectsArrayOperatorAndLeavesUnboundWhenReplacerMissing() {
        String sql = "select data ? array['a'] and x = ? and y = ?";
        String result = SqlUtils.replaceSqlParameter(sql, 0, i -> i == 0 ? "1" : null);
        Assert.assertEquals("select data ? array['a'] and x = 1 and y = null", result);
    }

    @Test
    public void testToNativeSqlHandlesNullAndDates() {
        String sql = SqlUtils.toNativeSql("select * from t where a=? and b=? and c=? and d=?", 1, true, new java.util.Date(0), null);
        Assert.assertTrue(sql.contains("1"));
        Assert.assertTrue(sql.contains("true"));
        Assert.assertTrue(sql.contains("1970-01-01 08:00:00") || sql.contains("1970-01-01 00:00:00"));
        Assert.assertTrue(sql.contains("null"));
    }

    @Test
    public void testReplaceSqlParameterEscapedQuotesAndOperatorBoundaries() {
        Assert.assertEquals("select 'it''s ?' and x=1",
                            SqlUtils.replaceSqlParameter("select 'it''s ?' and x=?", 0, i -> "1"));
        Assert.assertEquals("select \"a\"\"?\" and x=1",
                            SqlUtils.replaceSqlParameter("select \"a\"\"?\" and x=?", 0, i -> "1"));
        Assert.assertEquals("select ?",
                            SqlUtils.replaceSqlParameter("select ?", 0, i -> "?"));
        Assert.assertEquals("select 1",
                            SqlUtils.replaceSqlParameter("select ?", 0, i -> "1"));
        Assert.assertEquals("select x 1 key",
                            SqlUtils.replaceSqlParameter("select x ? key", 0, i -> "1"));
    }
}
