package org.hsweb.ezorm.meta.parser;

import org.hsweb.ezorm.meta.TableMetaData;

import java.sql.SQLException;
import java.util.List;

/**
 * Created by zhouhao on 16-6-5.
 */
public interface TableMetaParser {
    TableMetaData parse(String name);

    List<TableMetaData> parseAll() throws SQLException;
}
