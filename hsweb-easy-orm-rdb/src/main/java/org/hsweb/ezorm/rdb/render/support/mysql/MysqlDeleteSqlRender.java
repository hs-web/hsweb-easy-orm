package org.hsweb.ezorm.rdb.render.support.mysql;

import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.core.param.Param;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleWhereSqlBuilder;
import org.hsweb.ezorm.rdb.render.SqlAppender;
import org.hsweb.ezorm.rdb.render.support.simple.CommonSqlRender;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;

import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Created by zhouhao on 16-6-4.
 */
public class MysqlDeleteSqlRender extends CommonSqlRender<Param> {
    public MysqlDeleteSqlRender(Dialect dialect) {
        setDialect(dialect);
    }

    class SimpleDeleteSqlRenderProcess extends SimpleWhereSqlBuilder {
        private RDBTableMetaData metaData;
        private Param            param;
        private SqlAppender whereSql = new SqlAppender();

        public SimpleDeleteSqlRenderProcess(RDBTableMetaData metaData, Param param) {
            this.metaData = metaData;
            this.param = param;
            List<Term> terms = param.getTerms();
            terms = terms.stream().filter(term ->term.getColumn()==null|| !term.getColumn().contains(".")).collect(Collectors.toList());
            param.setTerms(terms);
            //解析查询条件
            buildWhere(metaData, "", terms, whereSql, new HashSet<>());
            if (!whereSql.isEmpty()) whereSql.removeFirst();
        }

        public SQL process() {
            SqlAppender appender = new SqlAppender();
            appender.add("DELETE ", metaData.getAlias(), " FROM ", metaData.getName(), " ", metaData.getAlias());
            if (whereSql.isEmpty()) {
                throw new UnsupportedOperationException("禁止执行未设置任何条件的删除操作!");
            }
            appender.add(" WHERE", " ").addAll(whereSql);
            String sql = appender.toString();
            SimpleSQL simpleSQL = new SimpleSQL(sql, param);
            return simpleSQL;
        }

        @Override
        public Dialect getDialect() {
            return dialect;
        }
    }

    @Override
    public SQL render(RDBTableMetaData metaData, Param param) {
        return new SimpleDeleteSqlRenderProcess(metaData, param).process();
    }

    private Dialect dialect;

    public Dialect getDialect() {
        return dialect;
    }

    public void setDialect(Dialect dialect) {
        this.dialect = dialect;
    }
}
