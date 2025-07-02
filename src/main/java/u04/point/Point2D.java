package u04.point;

/**
 * Create a simple immutable Java class (e.g., Point2D with x, y coordinates and methods like distanceTo, translate, rotate)
 */
public record Point2D(Double x, Double y) {

    public Double distanceTo(final Point2D point) {
        return Math.sqrt(Math.pow(this.x - point.x(), 2) + Math.pow(this.y - point.y(), 2));
    }

    public Point2D translate(final Double dx, final Double dy) {
        return new Point2D(x + dx, y + dy);
    }

    public Point2D rotate(double angleRadians) {
        double cos = Math.cos(angleRadians);
        double sin = Math.sin(angleRadians);
        double newX = x * cos - y * sin;
        double newY = x * sin + y * cos;
        return new Point2D(newX, newY);
    }

}
