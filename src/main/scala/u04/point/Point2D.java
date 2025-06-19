package scala.u04.point;

import scala.u04.datastructures.Functions;

/**
 * Create a simple immutable Java class (e.g., Point2D with x, y coordinates and methods like distanceTo, translate, rotate)
 */
public final class Point2D {
    final private Double x, y;
    Point2D(final Double x, final Double y) {
        this.x = x;
        this.y = y;
    }

    public Double getX() {
        return this.x;
    }

    public Double getY() {
        return this.y;
    }

    public Double distance(final Point2D point) {
        return Math.sqrt(Math.pow(this.x - point.getX(), 2) + Math.pow(this.y - point.getY(), 2));
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
