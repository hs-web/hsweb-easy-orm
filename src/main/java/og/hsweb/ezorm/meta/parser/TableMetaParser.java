package og.hsweb.ezorm.meta.parser;

import og.hsweb.ezorm.meta.TableMetaData;

/**
 * Created by zhouhao on 16-6-5.
 */
public interface TableMetaParser {
    TableMetaData parse(String name);
}
