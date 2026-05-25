package org.hswebframework.ezorm.rdb.supports.json;

/**
 * SPI for converting vendor-specific JSON holder objects into JSON text.
 *
 * <p>Implementations may live in optional database-driver integrations and are
 * discovered by {@link java.util.ServiceLoader}. This keeps the common JSON
 * codec independent from driver-specific reflection.</p>
 */
public interface JsonStringReader {

    boolean supports(Object data);

    String read(Object data) throws Exception;
}
