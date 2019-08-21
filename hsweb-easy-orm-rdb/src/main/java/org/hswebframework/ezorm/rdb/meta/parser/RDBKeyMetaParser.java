package org.hswebframework.ezorm.rdb.meta.parser;

import org.hswebframework.ezorm.rdb.meta.RDBKeyMetadata;

import java.util.List;

public interface RDBKeyMetaParser {

    List<RDBKeyMetadata> parseKeyByTable(String table);
}
