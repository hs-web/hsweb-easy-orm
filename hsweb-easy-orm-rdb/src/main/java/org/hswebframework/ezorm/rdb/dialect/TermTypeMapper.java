package org.hswebframework.ezorm.rdb.dialect;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SimpleSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlTemplateParser;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public interface TermTypeMapper {
    List<String> accept(String wherePrefix, Term term, RDBColumnMetadata column, String tableAlias);

    static TermTypeMapper sql(String sql) {
        return (wherePrefix, term, column, tableAlias) -> Collections.singletonList(sql);
    }

//    static TermTypeMapper sql(String sql, Object param) {
//
//        return (wherePrefix, term, column, tableAlias) -> {
//            SqlRequest request = SqlTemplateParser.parse(sql, param);
//
//            return new SqlAppender(((SimpleSqlRequest) request).toNativeSql());
//        };
//    }

}
