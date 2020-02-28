package org.hswebframework.ezorm.rdb.mapping.parser;

import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.apache.commons.beanutils.PropertyUtils;
import org.hswebframework.ezorm.rdb.mapping.annotation.ColumnType;
import org.hswebframework.ezorm.rdb.mapping.jpa.SimpleEntityPropertyDescriptor;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.junit.Assert;
import org.junit.Test;

import javax.persistence.Column;
import java.beans.PropertyDescriptor;
import java.sql.JDBCType;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.Assert.*;

public class DefaultDataTypeResolverTest {


    @Test
    @SneakyThrows
    public void test() {
        DefaultDataTypeResolver resolver = new DefaultDataTypeResolver();

        Map<String, PropertyDescriptor> descriptorMap = Stream.of(PropertyUtils.getPropertyDescriptors(Entity.class))
                .collect(Collectors.toMap(PropertyDescriptor::getName, Function.identity()));

        {
            PropertyDescriptor name = descriptorMap.get("name");

            DataType dataType = resolver.resolve(SimpleEntityPropertyDescriptor.of(name,null));
            assertNotNull(dataType);
            Assert.assertEquals(dataType.getId(), "varchar");
            Assert.assertEquals(dataType.getSqlType(), JDBCType.VARCHAR);
            Assert.assertEquals(dataType.getJavaType(), String.class);

        }

        {
            PropertyDescriptor property = descriptorMap.get("jsonArray");

            DataType dataType = resolver.resolve(SimpleEntityPropertyDescriptor.of(property,null));
            assertNotNull(dataType);
            Assert.assertEquals(dataType.getId(), "jsonb");
            Assert.assertEquals(dataType.getSqlType(), JDBCType.VARCHAR);

            Assert.assertEquals(dataType.getJavaType(), String.class);
        }

        {
            PropertyDescriptor property = descriptorMap.get("custom");

            DataType dataType = resolver.resolve(SimpleEntityPropertyDescriptor.of(property,null));
            assertNotNull(dataType);
            Assert.assertEquals(dataType.getId(), "custom");
            Assert.assertEquals(dataType.getSqlType(), JDBCType.CLOB);

            Assert.assertEquals(dataType.getJavaType(), String.class);
        }

        {
            PropertyDescriptor property = descriptorMap.get("id");

            DataType dataType = resolver.resolve(SimpleEntityPropertyDescriptor.of(property,null));
            assertNotNull(dataType);
            Assert.assertEquals(dataType.getId(), "varchar");
            Assert.assertEquals(dataType.getSqlType(), JDBCType.VARCHAR);

            Assert.assertEquals(dataType.getJavaType(), String.class);

        }

    }

    public interface InterfaceEntity<ID> {
        ID getId();
    }

    @Getter
    @Setter
    public static class GenericEntity<ID> implements InterfaceEntity<ID> {
        private ID id;

        @ColumnType(typeId = "jsonb", javaType =String.class)
        private List<String> jsonArray;

    }

    @Getter
    @Setter
    public static class Entity extends GenericEntity<String> {

        @Override
        @Column
        @ColumnType
        public String getId() {
            return super.getId();
        }

        @ColumnType
        private String name;


        @ColumnType(type = CustomType.class)
        private String custom;

    }

    public static class CustomType implements DataType {

        @Override
        public String getId() {
            return "custom";
        }

        @Override
        public String getName() {
            return "custom";
        }

        @Override
        public JDBCType getSqlType() {
            return JDBCType.CLOB;
        }

        @Override
        public Class<?> getJavaType() {
            return String.class;
        }
    }

}