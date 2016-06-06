package org.hsweb.ezorm.meta.parser;

import org.hsweb.ezorm.meta.TableMetaData;

/**
 * Created by zhouhao on 16-6-5.
 */
public interface TableMetaParser {
    TableMetaData parse(String name);
}
