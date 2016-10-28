package org.hsweb.ezorm.rdb.render.support.simple;

import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;
import org.hsweb.ezorm.rdb.meta.Correlation;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.core.param.Param;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * Created by zhouhao on 16-6-4.
 */
public abstract class CommonSqlRender<R extends Param> implements SqlRender<R> {
    protected Logger logger = LoggerFactory.getLogger(this.getClass());

    protected class OperationField {
        private String            tableName;
        private RDBColumnMetaData RDBColumnMetaData;

        public OperationField(String tableName, RDBColumnMetaData RDBColumnMetaData) {
            this.tableName = tableName;
            this.RDBColumnMetaData = RDBColumnMetaData;
            if (tableName == null) this.tableName = RDBColumnMetaData.getTableMetaData().getAlias();
        }

        public String getTableName() {
            return tableName;
        }

        public RDBColumnMetaData getRDBColumnMetaData() {
            return RDBColumnMetaData;
        }
    }

    protected List<OperationField> parseOperationField(RDBTableMetaData metaData, R param) {
        Set<String> includes = param.getIncludes(),
                excludes = param.getExcludes();
        boolean includesIsEmpty = includes.isEmpty(),
                excludesIsEmpty = excludes.isEmpty();
        List<OperationField> tmp = new ArrayList<>();
        if ((includesIsEmpty && excludesIsEmpty)) {
            metaData.getColumns().forEach(field -> tmp.add(new OperationField(null, field)));
            return tmp;
        }
        //指定了exclude,没有指定include
        if (includesIsEmpty && !excludesIsEmpty) {
            boolean hasSelf = false;
            for (String exclude : excludes) {
                if (exclude.contains(".")) {
                    includes.add(exclude.split("[.]")[0] + ".*");
                } else {
                    hasSelf = true;
                }
            }
            if (hasSelf) includes.add("*");
            includesIsEmpty = false;
        }
        if (!includesIsEmpty) {
            includes.forEach(include -> {
                if (excludes.contains(include)) return;
                if ("*".equals(include)) {
                    metaData.getColumns().forEach(field -> {
                        if (excludes.contains(field.getAlias()) || excludes.contains(field.getName()))
                            return;
                        tmp.add(new OperationField(null, field));
                    });
                    return;
                }
                if (include.contains(".")) {
                    String[] fieldInfo = include.split("[.]");
                    RDBTableMetaData table = metaData.getDatabaseMetaData().getTable(fieldInfo[0]);
                    String tname = null;
                    if (null == table) {
                        Correlation correlation = metaData.getCorrelation(fieldInfo[0]);
                        if (correlation != null){
                            table = metaData.getDatabaseMetaData().getTable(correlation.getTargetTable());
                            tname = correlation.getAlias();
                        }
                    } else {
                        tname = table.getAlias();
                    }
                    if (null == table) return;
                    if (fieldInfo[1].equals("*")) {
                        String finalName = tname;
                        table.getColumns().forEach(field -> {
                            if (excludes.contains(field.getFullAliasName()) || excludes.contains(field.getFullName())
                                    || excludes.contains(finalName + "." + field.getName())
                                    || excludes.contains(finalName + "." + field.getAlias()))
                                return;
                            tmp.add(new OperationField(finalName, field));
                        });
                        return;
                    } else {
                        RDBColumnMetaData field = metaData.findColumn(include);
                        if (null != field) {
                            if (excludes.contains(field.getFullAliasName()) || excludes.contains(field.getFullName()))
                                return;
                            tmp.add(new OperationField(tname, field));
                        }
                    }
                } else {
                    RDBColumnMetaData field = metaData.findColumn(include);
                    if (null != field) {
                        if (excludes.contains(field.getAlias()) || excludes.contains(field.getName()))
                            return;
                        tmp.add(new OperationField(field.getTableMetaData().getAlias(), field));
                    }
                }
            });
        }
        if (tmp.isEmpty()) throw new UnsupportedOperationException("未找到任何查询字段!");
        return tmp;
    }
}
