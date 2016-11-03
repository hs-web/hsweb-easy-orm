package org.hsweb.ezorm.core.meta.storage;

import org.hsweb.ezorm.core.meta.TableMetaData;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public class MapTableMetaDataStorage implements TableMetaDataStorage {
    private final Map<String, TableMetaData> nameTableMetaDataStorage  = new HashMap<>();
    private final Map<String, TableMetaData> aliasTableMetaDataStorage = new HashMap<>();

    @Override
    public <T extends TableMetaData> Set<T> getAllTableMetaData() {
        Set<T> all = new LinkedHashSet<>();
        all.addAll((Set) nameTableMetaDataStorage.values());
        return all;
    }

    @Override
    public <T extends TableMetaData> T getTableMetaData(String nameOrAlias) {
        T old = (T) nameTableMetaDataStorage.get(nameOrAlias);
        if (old == null) old = (T) aliasTableMetaDataStorage.get(nameOrAlias);
        return old;
    }

    @Override
    public <T extends TableMetaData> T removeTableMeta(String nameOrAlias) {
        T old = (T) nameTableMetaDataStorage.remove(nameOrAlias);
        T old2 = (T) aliasTableMetaDataStorage.remove(nameOrAlias);
        return old != null ? old : old2;
    }

    @Override
    public <T extends TableMetaData> T putTableMetaData(T table) {
        nameTableMetaDataStorage.put(table.getName(), table);
        if (!table.getName().equals(table.getAlias())) {
            nameTableMetaDataStorage.put(table.getAlias(), table);
        }
        return table;
    }

    @Override
    public void clear() {
        nameTableMetaDataStorage.clear();
        aliasTableMetaDataStorage.clear();
    }
}
