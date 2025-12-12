package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.junit.Test;

import java.util.function.Function;

import static org.junit.Assert.*;

public class SqlUtilsTest {

    @Test
    public void testCreateQuestionMarks(){

        {
            SqlFragments fragments = SqlUtils.createQuestionMarks(200);
            assertNotNull(fragments);
            System.out.println(fragments.toRequest().getSql());
            assertEquals(200,fragments.toRequest().getSql().split(",").length);
        }

        {
            SqlFragments fragments = SqlUtils.createQuestionMarks(512);
            assertNotNull(fragments);
            System.out.println(fragments.toRequest().getSql());
            assertEquals(512,fragments.toRequest().getSql().split(",").length);
        }

        {
            SqlFragments fragments = SqlUtils.createQuestionMarks(1024);
            assertNotNull(fragments);
            System.out.println(fragments.toRequest().getSql());
            assertEquals(1024,fragments.toRequest().getSql().split(",").length);
        }
    }

    @Test
    public void testReplaceSqlParameter_Basic() {
        String sql = "SELECT * FROM user WHERE id = ? AND name = ?";
        Function<Integer, String> replacer = i -> i == 0 ? "1" : "'test'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1 AND name = 'test'", result);
    }

    @Test
    public void testReplaceSqlParameter_EmptySql() {
        String sql = "";
        Function<Integer, String> replacer = i -> "value";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("", result);
    }

    @Test
    public void testReplaceSqlParameter_NoParameters() {
        String sql = "SELECT * FROM user";
        Function<Integer, String> replacer = i -> "value";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user", result);
    }

    @Test
    public void testReplaceSqlParameter_SingleQuoteString() {
        // 测试单引号字符串中的 ? 不应该被替换
        String sql = "SELECT * FROM user WHERE name = 'test?' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE name = 'test?' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_DoubleQuoteString() {
        // 测试双引号字符串中的 ? 不应该被替换
        String sql = "SELECT * FROM user WHERE name = \"test?\" AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE name = \"test?\" AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_SingleQuoteEscape() {
        // 测试单引号转义：SQL 中用 '' 表示一个单引号
        // 这是当前代码的一个潜在问题：没有正确处理转义
        String sql = "SELECT * FROM user WHERE name = 'It''s test' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        // 期望：字符串内的 ? 不应该被替换，即使有转义的单引号
        assertEquals("SELECT * FROM user WHERE name = 'It''s test' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_SingleQuoteEscapeWithQuestionMark() {
        // 测试转义单引号后的问号不应该被替换
        // 注意：当前实现可能存在问题，'It''s test?' 中的 ? 可能会被错误替换
        // 因为 '' 被当作两个独立的引号处理，而不是转义
        String sql = "SELECT * FROM user WHERE name = 'It''s test?' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        // 期望：字符串内的 ? 不应该被替换，即使有转义的单引号
        // 但实际上由于转义处理问题，可能会被替换
        assertEquals("SELECT * FROM user WHERE name = 'It''s test?' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_SingleQuoteEscapeBug() {
        // 这个测试验证转义单引号的处理：
        // SQL 中 'It''s?' 应该是一个完整的字符串（It's?），
        // 字符串内的 ? 不应该被替换
        String sql = "SELECT * FROM user WHERE name = 'It''s?' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        // 期望：'It''s?' 中的 ? 不应该被替换，只有 id = ? 中的 ? 应该被替换
        assertEquals("SELECT * FROM user WHERE name = 'It''s?' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_SingleQuoteEscapeComplex() {
        // 测试复杂的转义场景：多个转义单引号和问号
        String sql = "SELECT * FROM user WHERE name = 'It''s test? value' AND id = ? AND desc = 'test''more?'";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        // 期望：字符串内的 ? 都不应该被替换
        assertEquals("SELECT * FROM user WHERE name = 'It''s test? value' AND id = 1 AND desc = 'test''more?'", result);
    }

    @Test
    public void testReplaceSqlParameter_MultipleSingleQuoteEscapes() {
        // 测试多个转义的单引号
        String sql = "SELECT * FROM user WHERE name = 'test''value''more' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE name = 'test''value''more' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_DoubleQuoteEscape() {
        // 测试双引号转义：SQL 中用 "" 表示一个双引号
        String sql = "SELECT * FROM user WHERE name = \"test\"\"value\" AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE name = \"test\"\"value\" AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_LineComment() {
        // 测试行注释中的 ? 不应该被替换
        String sql = "SELECT * FROM user WHERE id = ? -- comment with ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1 -- comment with ?", result);
    }

    @Test
    public void testReplaceSqlParameter_BlockComment() {
        // 测试块注释中的 ? 不应该被替换
        String sql = "SELECT * FROM user WHERE id = ? /* comment with ? */ AND name = ?";
        Function<Integer, String> replacer = i -> i == 0 ? "1" : "'test'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1 /* comment with ? */ AND name = 'test'", result);
    }

    @Test
    public void testReplaceSqlParameter_JsonbOperator() {
        // 测试 JSONB 操作符 ?| ?& ?! 不应该被替换
        String sql = "SELECT * FROM user WHERE data ?| array['key1', 'key2'] AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?| array['key1', 'key2'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_JsonbOperator_And() {
        String sql = "SELECT * FROM user WHERE data ?& array['key1'] AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?& array['key1'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_JsonbOperator_Not() {
        String sql = "SELECT * FROM user WHERE data ?! 'key1' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?! 'key1' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_MultipleParameters() {
        String sql = "SELECT * FROM user WHERE id = ? AND name = ? AND age = ?";
        Function<Integer, String> replacer = i -> {
            if (i == 0) return "1";
            if (i == 1) return "'John'";
            return "25";
        };
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1 AND name = 'John' AND age = 25", result);
    }

    @Test
    public void testReplaceSqlParameter_ComplexCase() {
        // 复杂场景：包含字符串、注释、JSONB 操作符和参数
        String sql = "SELECT * FROM user WHERE name = 'test?' AND id = ? -- comment ?\n" +
                "AND data ?| array['key'] AND value = ? /* block ? */";
        Function<Integer, String> replacer = i -> i == 0 ? "1" : "'value'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        String expected = "SELECT * FROM user WHERE name = 'test?' AND id = 1 -- comment ?\n" +
                "AND data ?| array['key'] AND value = 'value' /* block ? */";
        assertEquals(expected, result);
    }

    @Test
    public void testReplaceSqlParameter_NestedQuotes() {
        // 测试嵌套引号
        String sql = "SELECT * FROM user WHERE name = 'test \"inner\" value' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE name = 'test \"inner\" value' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_UnclosedString() {
        // 测试未闭合的字符串（边界情况）
        String sql = "SELECT * FROM user WHERE name = 'test AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        // 在这种情况下，字符串未闭合，? 在字符串内，不应该被替换
        assertEquals("SELECT * FROM user WHERE name = 'test AND id = ?", result);
    }

    @Test
    public void testReplaceSqlParameter_QuestionMarkAtEnd() {
        // 测试 ? 在 SQL 末尾
        String sql = "SELECT * FROM user WHERE id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_QuestionMarkAtStart() {
        // 测试 ? 在 SQL 开头（不太可能，但测试边界情况）
        String sql = "? AND id = 1";
        Function<Integer, String> replacer = i -> "SELECT * FROM user WHERE name = 'test'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE name = 'test' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_EstimatedExtraLen() {
        // 测试 estimatedExtraLen 参数
        String sql = "SELECT * FROM user WHERE id = ?";
        Function<Integer, String> replacer = i -> "1234567890"; // 10 个字符，比 ? 长 9 个
        String result = SqlUtils.replaceSqlParameter(sql, 10, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1234567890", result);
    }

    @Test
    public void testToNativeSql() {
        // 测试 toNativeSql 方法
        String sql = "SELECT * FROM user WHERE id = ? AND name = ?";
        String result = SqlUtils.toNativeSql(sql, 1, "test");
        assertEquals("SELECT * FROM user WHERE id = 1 AND name = 'test'", result);
    }

    @Test
    public void testToNativeSql_WithNull() {
        String sql = "SELECT * FROM user WHERE id = ? AND name = ?";
        String result = SqlUtils.toNativeSql(sql, null, "test");
        assertEquals("SELECT * FROM user WHERE id = null AND name = 'test'", result);
    }

    @Test
    public void testToNativeSql_WithDate() {
        java.util.Date date = new java.util.Date(1609459200000L); // 2021-01-01 00:00:00
        String sql = "SELECT * FROM user WHERE create_time = ?";
        String result = SqlUtils.toNativeSql(sql, date);
        // 日期格式应该是 'yyyy-MM-dd HH:mm:ss'
        assertTrue(result.contains("2021-01-01"));
    }

    @Test
    public void testToNativeSql_WithStringContainingSingleQuote() {
        // 测试包含单引号的字符串参数
        String sql = "SELECT * FROM user WHERE name = ?";
        String result = SqlUtils.toNativeSql(sql, "O'Brien");
        // 注意：toNativeSql 只是简单包装，不会转义单引号，这可能是另一个潜在问题
        assertEquals("SELECT * FROM user WHERE name = 'O'Brien'", result);
    }

    // ========== PostgreSQL 操作符测试 ==========

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_Single() {
        // 测试 PostgreSQL JSONB ? 操作符（检查键是否存在）
        String sql = "SELECT * FROM user WHERE data ? 'key1' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ? 'key1' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_WithArray() {
        // 测试 PostgreSQL JSONB ? 操作符与 array[...]
        String sql = "SELECT * FROM user WHERE data ? array['key1', 'key2'] AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ? array['key1', 'key2'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_WithBrackets() {
        // 测试 PostgreSQL JSONB ? 操作符前面有括号
        String sql = "SELECT * FROM user WHERE (data) ? 'key1' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE (data) ? 'key1' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_WithArrayBrackets() {
        // 测试 PostgreSQL JSONB ? 操作符前面有方括号
        String sql = "SELECT * FROM user WHERE data[0] ? 'key1' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data[0] ? 'key1' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_Or() {
        // 测试 PostgreSQL JSONB ?| 操作符（检查任意键是否存在）
        String sql = "SELECT * FROM user WHERE data ?| array['key1', 'key2'] AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?| array['key1', 'key2'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_And() {
        // 测试 PostgreSQL JSONB ?& 操作符（检查所有键是否存在）
        String sql = "SELECT * FROM user WHERE data ?& array['key1', 'key2'] AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?& array['key1', 'key2'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_Not() {
        // 测试 PostgreSQL JSONB ?! 操作符（检查键是否不存在）
        String sql = "SELECT * FROM user WHERE data ?! 'key1' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?! 'key1' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_Multiple() {
        // 测试多个 PostgreSQL JSONB 操作符
        String sql = "SELECT * FROM user WHERE data ? 'key1' AND data2 ?| array['k1'] AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ? 'key1' AND data2 ?| array['k1'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_Complex() {
        // 测试复杂的 PostgreSQL JSONB 操作符场景
        String sql = "SELECT * FROM user WHERE jsonb_column ? 'key' AND id = ? AND name = ? AND other ?| array['a', 'b']";
        Function<Integer, String> replacer = i -> i == 0 ? "1" : "'test'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE jsonb_column ? 'key' AND id = 1 AND name = 'test' AND other ?| array['a', 'b']", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_NotOperator() {
        // 测试 ? 不是操作符的情况（前面是操作符如 =, > 等）
        String sql = "SELECT * FROM user WHERE id = ? AND name > ?";
        Function<Integer, String> replacer = i -> i == 0 ? "1" : "2";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id = 1 AND name > 2", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_InClause() {
        // 测试 IN 子句中的 ? 应该被替换
        String sql = "SELECT * FROM user WHERE id IN (?, ?, ?) AND data ? 'key'";
        Function<Integer, String> replacer = i -> i < 3 ? String.valueOf(i + 1) : "unused";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (1, 2, 3) AND data ? 'key'", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_WithSpaces() {
        // 测试操作符前后有多个空格
        String sql = "SELECT * FROM user WHERE data   ?   'key1' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data   ?   'key1' AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_WithTableAlias() {
        // 测试带表别名的情况
        String sql = "SELECT * FROM user u WHERE u.data ? 'key1' AND u.id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user u WHERE u.data ? 'key1' AND u.id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_PostgresJsonbOperator_WithFunction() {
        // 测试函数调用后的操作符
        String sql = "SELECT * FROM user WHERE jsonb_extract_path(data, 'path') ? 'key' AND id = ?";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE jsonb_extract_path(data, 'path') ? 'key' AND id = 1", result);
    }

    // ========== 函数参数预编译测试 ==========

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter() {
        // 测试 array[?,?] 函数参数中的 ? 应该被替换
        String sql = "SELECT * FROM user WHERE id IN (array[?, ?])";
        Function<Integer, String> replacer = i -> i == 0 ? "1" : "2";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1, 2])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_Multiple() {
        // 测试多个 array[?,?] 函数参数
        String sql = "SELECT * FROM user WHERE id IN (array[?, ?]) AND name IN (array[?, ?])";
        Function<Integer, String> replacer = i -> {
            if (i < 2) return String.valueOf(i + 1);
            return "'name" + (i - 1) + "'";
        };
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1, 2]) AND name IN (array['name1', 'name2'])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_WithOperator() {
        // 测试 array[?,?] 和操作符 ? 同时存在
        String sql = "SELECT * FROM user WHERE data ?| array[?, ?] AND id = ?";
        Function<Integer, String> replacer = i -> {
            if (i < 2) return "'key" + (i + 1) + "'";
            return "1";
        };
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE data ?| array['key1', 'key2'] AND id = 1", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_Single() {
        // 测试 array[?] 单个参数
        String sql = "SELECT * FROM user WHERE id IN (array[?])";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_Empty() {
        // 测试 array[] 空数组（不应该有参数）
        String sql = "SELECT * FROM user WHERE id IN (array[]) AND name = ?";
        Function<Integer, String> replacer = i -> "'test'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[]) AND name = 'test'", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_Many() {
        // 测试 array[?,?,?,?] 多个参数
        String sql = "SELECT * FROM user WHERE id IN (array[?, ?, ?, ?])";
        Function<Integer, String> replacer = i -> String.valueOf(i + 1);
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1, 2, 3, 4])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_Nested() {
        // 测试嵌套的 array 函数
        String sql = "SELECT * FROM user WHERE id IN (array[?, array[?, ?]])";
        Function<Integer, String> replacer = i -> String.valueOf(i + 1);
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1, array[2, 3]])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_WithString() {
        // 测试 array 函数中包含字符串参数
        String sql = "SELECT * FROM user WHERE tags IN (array[?, ?])";
        Function<Integer, String> replacer = i -> i == 0 ? "'tag1'" : "'tag2'";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE tags IN (array['tag1', 'tag2'])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_WithNumber() {
        // 测试 array 函数中包含数字参数
        String sql = "SELECT * FROM user WHERE scores IN (array[?, ?])";
        Function<Integer, String> replacer = i -> i == 0 ? "100" : "200";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE scores IN (array[100, 200])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_Complex() {
        // 测试复杂的 array 函数场景
        String sql = "SELECT * FROM user WHERE id IN (array[?, ?]) AND data ?| array[?, ?] AND name = ?";
        Function<Integer, String> replacer = i -> {
            if (i < 2) return String.valueOf(i + 1);  // 第一个 array[?, ?] 的参数：1, 2
            if (i < 4) return "'key" + i + "'";  // 第二个 array[?, ?] 的参数：i=2 -> 'key2', i=3 -> 'key3'
            return "'test'";  // name = ? 的参数
        };
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1, 2]) AND data ?| array['key2', 'key3'] AND name = 'test'", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_WithSpaces() {
        // 测试 array[?, ?] 中有空格的情况
        String sql = "SELECT * FROM user WHERE id IN (array[ ? , ? ])";
        Function<Integer, String> replacer = i -> String.valueOf(i + 1);
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[ 1 , 2 ])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_WithOtherFunctions() {
        // 测试与其他函数一起使用
        String sql = "SELECT * FROM user WHERE id IN (array[?, ?]) AND LENGTH(name) = ?";
        Function<Integer, String> replacer = i -> {
            if (i < 2) return String.valueOf(i + 1);
            return "10";
        };
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE id IN (array[1, 2]) AND LENGTH(name) = 10", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_NotOperator() {
        // 测试确保 array[...] 中的 ? 不会被误判为操作符
        // 这个测试很重要：array[?] 中的 ? 应该被替换，而不是被当作操作符
        String sql = "SELECT * FROM user WHERE data ? 'key' AND ids IN (array[?])";
        Function<Integer, String> replacer = i -> "1";
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        // data ? 'key' 中的 ? 是操作符，不应该被替换
        // array[?] 中的 ? 是参数，应该被替换
        assertEquals("SELECT * FROM user WHERE data ? 'key' AND ids IN (array[1])", result);
    }

    @Test
    public void testReplaceSqlParameter_ArrayFunctionParameter_MixedWithOperator() {
        // 测试混合场景：操作符 ? 和 array[?] 参数同时存在
        String sql = "SELECT * FROM user WHERE jsonb_data ? 'key' AND ids IN (array[?, ?]) AND other ?| array['a']";
        Function<Integer, String> replacer = i -> String.valueOf(i + 1);
        String result = SqlUtils.replaceSqlParameter(sql, 0, replacer);
        assertEquals("SELECT * FROM user WHERE jsonb_data ? 'key' AND ids IN (array[1, 2]) AND other ?| array['a']", result);
    }
}