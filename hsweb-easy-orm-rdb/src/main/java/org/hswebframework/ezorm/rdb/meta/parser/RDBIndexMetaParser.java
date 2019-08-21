package org.hswebframework.ezorm.rdb.meta.parser;

import org.hswebframework.ezorm.rdb.meta.RDBIndexMetaData;

import java.util.List;

public interface RDBIndexMetaParser {

    List<RDBIndexMetaData> parseIndexByTable(String name);
}
