package org.hsweb.ezorm.render.support.simple;

import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.Correlation;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.param.QueryParam;
import org.hsweb.ezorm.render.Dialect;
import org.hsweb.ezorm.render.SqlAppender;

import java.util.*;

/**
 * Created by zhouhao on 16-5-17.
 */
public class SimpleSelectTotalSqlRender extends CommonSqlRender<QueryParam> {

    private Dialect dialect;

    public SimpleSelectTotalSqlRender(Dialect dialect) {
        this.dialect = dialect;
    }

    public Dialect getDialect() {
        return dialect;
    }

    public void setDialect(Dialect dialect) {
        this.dialect = dialect;
    }

    class SimpleSelectSqlRenderProcess extends SimpleWhereSqlBuilder {
        private TableMetaData metaData;
        private QueryParam param;
        private SqlAppender whereSql = new SqlAppender();
        private Set<String> needSelectTable = new LinkedHashSet<>();

        public SimpleSelectSqlRenderProcess(TableMetaData metaData, QueryParam param) {
            this.metaData = metaData;
            this.param = param;
            //解析要查询的字段
            //解析查询条件
            buildWhere(metaData, "", param.getTerms(), whereSql, needSelectTable);
            if (!whereSql.isEmpty()) whereSql.removeFirst();
        }

        public SQL process() {
            SqlAppender appender = new SqlAppender();
            appender.add("SELECT count(0) as ", dialect.getQuoteStart(), "total", dialect.getQuoteEnd());
            appender.add(" FROM ", metaData.getName(), " ", metaData.getAlias());
            //生成join
            needSelectTable.forEach(table -> {
                if (table.equals(metaData.getName())) return;
                Correlation correlation = metaData.getCorrelation(table);
                if (correlation != null) {
                    appender.add(" ", correlation.getJoin(), " "
                            , correlation.getTargetTable(), " ", correlation.getAlias()
                            , " ON ");
                    SqlAppender joinOn = new SqlAppender();
                    buildWhere(metaData.getDatabaseMetaData().getTable(correlation.getTargetTable()),
                            "", correlation.getTerms(), joinOn, new HashSet());
                    if (!joinOn.isEmpty()) joinOn.removeFirst();
                    appender.addAll(joinOn);
                }
            });
            if (!whereSql.isEmpty())
                appender.add(" WHERE ", "").addAll(whereSql);
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
    public SQL render(TableMetaData metaData, QueryParam param) {
        return new SimpleSelectSqlRenderProcess(metaData, param).process();
    }
}
