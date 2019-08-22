package org.hswebframework.ezorm.rdb.meta.parser;

import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;

import java.sql.SQLException;
import java.util.List;

/**
 * Created by zhouhao on 16-6-5.
 */
@Deprecated
public interface TableMetaParser {
    RDBTableMetaData parse(String name);

    boolean tableExists(String name);

    List<RDBTableMetaData> parseAll() throws SQLException;
}
